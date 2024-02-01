#include <wasmedge/wasmedge.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define DELIMITER ","

union Input {
    unsigned long long i64;
    unsigned int i32;
    float f32;
    double f64;
};

typedef struct Expected {
    char* type;
    char* value;
} Expected;

typedef struct InvokeResult {
    WasmEdge_Result Res;
    WasmEdge_Value* Returns;
    int returnCount;
} InvokeResult;

void PrintWasmEdgeValue (WasmEdge_Value value) {
    switch (value.Type) {
        case WasmEdge_ValType_I32:
            fprintf(stderr, "(I32.CONST 0x%x)", WasmEdge_ValueGetI32(value));
            break;
        case WasmEdge_ValType_I64:
            fprintf(stderr, "(I64.CONST 0x%lx)", WasmEdge_ValueGetI64(value));
            break;
        case WasmEdge_ValType_F32:
            fprintf(stderr, "(F32.CONST 0x%x)", WasmEdge_ValueGetI32(value));
            break;
        case WasmEdge_ValType_F64:
            fprintf(stderr, "(F64.CONST 0x%lx)", WasmEdge_ValueGetI64(value));
            break;
        case WasmEdge_ValType_FuncRef:
            if (WasmEdge_ValueGetFuncRef(value) == NULL)
                fprintf(stderr, "(FUNCREF NULL)");
            else
                fprintf(stderr, "(FUNCREF %p)", WasmEdge_ValueGetFuncRef(value));
            break;
        case WasmEdge_ValType_ExternRef:
            if (WasmEdge_ValueGetExternRef(value) == NULL)
                fprintf(stderr, "(EXTERNREF NULL)");
            else
                fprintf(stderr, "(EXTERNREF %p)", WasmEdge_ValueGetExternRef(value));
            break;
        case WasmEdge_ValType_V128:
            int128_t v128 = WasmEdge_ValueGetV128(value);
            uint64_t low = (uint64_t)v128;
            uint64_t high = (uint64_t)(v128 >> 64);
            fprintf(stderr, "(V128.CONST 0x%lx 0x%lx)", low, high);
            break;
        default:
            break;
    }
}

void PrintInvokeMessage (char* modName, const char* funcName, WasmEdge_Value* Params, int paramCount) {
    fprintf(stderr, "[Invoking %s %s", modName, funcName);

    for (int i=0; i<paramCount; ++i) {
        PrintWasmEdgeValue(Params[i]);

        if (i < paramCount - 1)
            fprintf(stderr, ", ");
    }

    fprintf(stderr, "]\n");
}

void PrintFailAssertReturn (WasmEdge_Value* actual, Expected* expected, const size_t count) {
    fprintf(stderr, " Fail!\n Expected: ");

    for (int i=0; i<count; ++i) {
        fprintf(stderr, "(%s %s)", expected[i].type, expected[i].value);

        if (i < count - 1)
            fprintf(stderr, ", ");
    }

    fprintf(stderr, "\n Actual: ");

    for (int i=0; i<count; ++i) {
        PrintWasmEdgeValue(actual[i]);

        if (i < count - 1)
            fprintf(stderr, ", ");
    }

    fprintf(stderr, "\n\n");
}

void PrintCheckErrorFailed () {
    fprintf(stderr, " Fail!\n Expected: Error\n Actual: Returned with no error\n\n");
}

void ParseValues (WasmEdge_Value* Values, int count) {
    for (int i=0; i<count; ++i) {
        char *type = strtok(NULL, DELIMITER);
        char *value = strtok(NULL, DELIMITER);

        union Input binary = { strtoull(value, NULL, 10) };

        if (strcmp(type, "i32") == 0)
            Values[i] = WasmEdge_ValueGenI32(binary.i32);
        if (strcmp(type, "i64") == 0)
            Values[i] = WasmEdge_ValueGenI64(binary.i64);
        if (strcmp(type, "f32") == 0)
            Values[i] = WasmEdge_ValueGenF32(binary.f32);
        if (strcmp(type, "f64") == 0)
            Values[i] = WasmEdge_ValueGenF64(binary.f64);
        if (strcmp(type, "funcref") == 0)
            Values[i] = (strcmp(value, "null") == 0)
            ? WasmEdge_ValueGenFuncRef(NULL)
            : WasmEdge_ValueGenFuncRef((void*)binary.i64 + 0x100000000ULL);
        if (strcmp(type, "externref") == 0)
            Values[i] = (strcmp(value, "null") == 0)
            ? WasmEdge_ValueGenExternRef(NULL)
            : WasmEdge_ValueGenExternRef((void*)binary.i64 + 0x100000000ULL);
        if (strcmp(type, "v128") == 0) {
            value = strtok(NULL, DELIMITER);
            int128_t v128 = (binary.i64 + ((int128_t)strtoull(value, NULL, 10) << 64));
            Values[i] = WasmEdge_ValueGenV128(v128);
        }
    }
}

void ParseExpected (Expected* expected, int count) {
    for (int i=0; i<count; ++i) {
        char *type = strtok(NULL, DELIMITER);
        char *value = strtok(NULL, DELIMITER);

        expected[i].type = malloc(strlen(type) + 1);
        expected[i].value = malloc(strlen(value) + 1);

        strcpy(expected[i].type, type);
        strcpy(expected[i].value, value);
    }
}

bool CheckValues (WasmEdge_Value* actual, Expected* expected, const size_t count) {
    for (int i=0; i<count; ++i) {
        union Input binary = { strtoull(expected[i].value, NULL, 10) };

        if (strcmp(expected[i].type, "i32") == 0 && (actual[i].Type != WasmEdge_ValType_I32 || binary.i32 != WasmEdge_ValueGetI32(actual[i]))) {
            return false;
        }

        if (strcmp(expected[i].type, "i64") == 0 && (actual[i].Type != WasmEdge_ValType_I64 || binary.i64 != WasmEdge_ValueGetI64(actual[i]))) {
            return false;
        }

        if (strcmp(expected[i].type, "f32") == 0) {
            int actualBin = WasmEdge_ValueGetI32(actual[i]);

            if (actual[i].Type == WasmEdge_ValType_F32 && strcmp(expected[i].value, "nan:canonical") == 0) {
                if (actualBin == 0x7fc00000 || actualBin == 0xffc00000)
                    continue;
            }

            if (actual[i].Type == WasmEdge_ValType_F32 && strcmp(expected[i].value, "nan:arithmetic") == 0) {
                if ((actualBin & 0x7fc00000) == 0x7fc00000)
                    continue;
            }

            if (actual[i].Type != WasmEdge_ValType_F32 || binary.i32 != WasmEdge_ValueGetI32(actual[i]))
                return false;
        }

        if (strcmp(expected[i].type, "f64") == 0) {
            long long actualBin = WasmEdge_ValueGetI64(actual[i]);

            if (actual[i].Type == WasmEdge_ValType_F64 && strcmp(expected[i].value, "nan:canonical") == 0) {
                if (actualBin == 0x7ff8000000000000LL || actualBin == 0xfff8000000000000LL)
                    continue;
            }

            if (actual[i].Type == WasmEdge_ValType_F64 && strcmp(expected[i].value, "nan:arithmetic") == 0) {
                if ((actualBin & 0x7ff8000000000000LL) == 0x7ff8000000000000LL)
                    continue;
            }

            if (actual[i].Type != WasmEdge_ValType_F64 || binary.i64 != WasmEdge_ValueGetI64(actual[i]))
                return false;
        }

        if (strcmp(expected[i].type, "funcref") == 0) {
            if (actual[i].Type == WasmEdge_ValType_FuncRef) {
                if (strcmp(expected[i].value, "null") == 0 && WasmEdge_ValueGetFuncRef(actual[i]) == NULL)
                    continue;
                if (strcmp(expected[i].value, "null") != 0 && WasmEdge_ValueGetFuncRef(actual[i]) != NULL)
                    continue;
            }

            return false;
        }

        if (strcmp(expected[i].type, "externref") == 0) {
            if (actual[i].Type == WasmEdge_ValType_ExternRef) {
                if (strcmp(expected[i].value, "null") == 0) {
                    if (WasmEdge_ValueGetExternRef(actual[i]) == NULL)
                        continue;
                }
                else {
                    if (binary.i64 + 0x100000000ULL == (unsigned long long)WasmEdge_ValueGetExternRef(actual[i]))
                        continue;
                }
            }

            return false;
        }

        if (strcmp(expected[i].type, "v128") == 0) {
            if (actual[i].Type != WasmEdge_ValType_V128)
                continue;

            char* lane = expected[i].value;
            size_t n = strlen(lane);

            for (int i=0; i<n; ++i) {
                if (*lane == '!')
                    *lane = ',';
                lane++;
            }

            size_t numLanes = strtoul (strtok(expected[i].value, DELIMITER), NULL, 10);

            WasmEdge_Value actualLanes[numLanes];
            Expected expectedLanes[numLanes];

            ParseExpected (expectedLanes, numLanes);
            char* laneType = expectedLanes[0].type;

            lane = expected[i].value;

            for (int i=0; i<n; ++i) {
                if (*lane == 0)
                    *lane = ',';
                lane++;
            }

            int128_t v128 = WasmEdge_ValueGetV128(actual[i]);

            if (strcmp(laneType, "i64") == 0) {
                actualLanes[0] = WasmEdge_ValueGenI64((int64_t)v128);
                actualLanes[1] = WasmEdge_ValueGenI64((int64_t)(v128 >> 64));
            }

            else if (laneType[0] == 'i') {
                size_t bitCount = 128 / numLanes;

                for (int i=0; i<numLanes; ++i) {
                    actualLanes[i] = WasmEdge_ValueGenI32((int32_t)v128 & ((0x1LL << bitCount) - 1));
                    v128 >>= bitCount;
                }
            }

            union Input binary;

            if (strcmp(laneType, "f32") == 0) {
                binary.i32 = (int32_t)v128;
                actualLanes[0] = WasmEdge_ValueGenF32(binary.f32);
                binary.i32 = (int32_t)(v128 >> 32);
                actualLanes[1] = WasmEdge_ValueGenF32(binary.f32);
                binary.i32 = (int32_t)(v128 >> 64);
                actualLanes[2] = WasmEdge_ValueGenF32(binary.f32);
                binary.i32 = (int32_t)(v128 >> 96);
                actualLanes[3] = WasmEdge_ValueGenF32(binary.f32);
            }

            if (strcmp(laneType, "f64") == 0) {
                binary.i64 = (int64_t)v128;
                actualLanes[0] = WasmEdge_ValueGenF64(binary.f64);
                binary.i64 = (int64_t)(v128 >> 64);
                actualLanes[1] = WasmEdge_ValueGenF64(binary.f64);
            }

            if (!CheckValues (actualLanes, expectedLanes, numLanes))
                return false;

            continue;
        }
    }

    return true;
}

InvokeResult Invoke (WasmEdge_VMContext *VMCtx) {
    char* modStr = strtok(NULL, DELIMITER);
    WasmEdge_String modName = WasmEdge_StringCreateByCString(modStr);

    char* fieldStr = strtok(NULL, DELIMITER);
    WasmEdge_String funcName = WasmEdge_StringCreateByCString(fieldStr);

    const WasmEdge_FunctionTypeContext *FuncType =
        WasmEdge_VMGetFunctionTypeRegistered(VMCtx, modName, funcName);

    const size_t paramCount = WasmEdge_FunctionTypeGetParametersLength(FuncType);
    const size_t returnCount = WasmEdge_FunctionTypeGetReturnsLength(FuncType);

    WasmEdge_Value Params[paramCount];
    ParseValues(Params, paramCount);

    PrintInvokeMessage(modStr, fieldStr, Params, paramCount);

    WasmEdge_Value Returns[returnCount];

    WasmEdge_Result Res = WasmEdge_VMExecuteRegistered(VMCtx, modName, funcName, Params, paramCount, Returns, returnCount);

    WasmEdge_StringDelete(modName);
    WasmEdge_StringDelete(funcName);

    InvokeResult result = { Res, malloc(sizeof(WasmEdge_Value) * returnCount), returnCount };
    memcpy(result.Returns, Returns, sizeof(WasmEdge_Value) * returnCount);

    return result;
}

int main(int argc, char** argv) {
    WasmEdge_ConfigureContext *ConfCxt = WasmEdge_ConfigureCreate();
    WasmEdge_VMContext *VMCtx = WasmEdge_VMCreate(ConfCxt, NULL);

    WasmEdge_VMRegisterModuleFromFile(VMCtx, WasmEdge_StringCreateByCString("spectest"), "./spectest.wasm");

    FILE *fptr = fopen(argv[1], "r");

    char buffer[1024];

    unsigned int correct = 0;

    while (fgets(buffer, 1024, fptr)) {
        buffer[strcspn(buffer, "\n")] = 0;

        char *ty = strtok(buffer, DELIMITER);

        if (strcmp(ty, "mon") == 0) {
            char *name = strtok(NULL, DELIMITER);
            char *path = strtok(NULL, DELIMITER);

            WasmEdge_VMRegisterModuleFromFile(VMCtx, WasmEdge_StringCreateByCString(name), path);
        }

        if (strcmp(ty, "act") == 0) {
            Invoke(VMCtx);
        }

        if (strcmp(ty, "ari") == 0) {
            InvokeResult result = Invoke(VMCtx);

            if (WasmEdge_ResultOK(result.Res)) {
                Expected expected[result.returnCount];
                ParseExpected(expected, result.returnCount);

                if (CheckValues(result.Returns, expected, result.returnCount)) {
                    correct++;
                }
                else {
                    PrintFailAssertReturn(result.Returns, expected, result.returnCount);
                }
            }
        }

        if (strcmp(ty, "art") == 0) {
            WasmEdge_String modName = WasmEdge_StringCreateByCString(strtok(NULL, DELIMITER));

            char* fieldStr = strtok(NULL, DELIMITER);
            WasmEdge_String funcName = WasmEdge_StringCreateByCString(fieldStr);

            WasmEdge_StoreContext *StoreCtx = WasmEdge_VMGetStoreContext(VMCtx);
            const WasmEdge_ModuleInstanceContext *ModCtx = WasmEdge_StoreFindModule(StoreCtx, modName);
            WasmEdge_GlobalInstanceContext *GlobCtx = WasmEdge_ModuleInstanceFindGlobal(ModCtx, funcName);

            fprintf(stderr, "[Getting %s]\n", fieldStr);

            if (GlobCtx != NULL) {
                WasmEdge_Value actual[1] = { WasmEdge_GlobalInstanceGetValue(GlobCtx) };

                Expected expected[1];
                ParseExpected(expected, 1);

                if (CheckValues(actual, expected, 1))
                    correct++;
                else
                    PrintFailAssertReturn(actual, expected, 1);
            }
        }

        if (strcmp(ty, "ati") == 0) {
            InvokeResult result = Invoke(VMCtx);

            if (!WasmEdge_ResultOK(result.Res)) {
                correct++;
                continue;
            }

            PrintCheckErrorFailed();
        }

        if (strcmp(ty, "aiv") == 0) {
            char* path = strtok(NULL, DELIMITER);

            WasmEdge_Result Res = WasmEdge_VMLoadWasmFromFile(VMCtx, path);
            if (!WasmEdge_ResultOK(Res)) {
                correct++;
                continue;
            }

            Res = WasmEdge_VMValidate(VMCtx);
            if (!WasmEdge_ResultOK(Res)) {
                correct++;
                continue;
            }

            PrintCheckErrorFailed();
        }

        if (strcmp(ty, "ama") == 0) {
            char* path = strtok(NULL, DELIMITER);

            WasmEdge_Result Res = WasmEdge_VMLoadWasmFromFile(VMCtx, path);
            if (!WasmEdge_ResultOK(Res)) {
                correct++;
                continue;
            }

            PrintCheckErrorFailed();
        }

        if (strcmp(ty, "aun") == 0) {
            char* path = strtok(NULL, DELIMITER);

            WasmEdge_Result Res = WasmEdge_VMLoadWasmFromFile(VMCtx, path);
            if (!WasmEdge_ResultOK(Res)) {
                correct++;
                continue;
            }

            Res = WasmEdge_VMValidate(VMCtx);
            if (!WasmEdge_ResultOK(Res)) {
                correct++;
                continue;
            }

            Res = WasmEdge_VMInstantiate(VMCtx);
            if (!WasmEdge_ResultOK(Res)) {
                correct++;
                continue;
            }

            PrintCheckErrorFailed();
        }
    }

    fclose(fptr);

    WasmEdge_VMDelete(VMCtx);
    WasmEdge_ConfigureDelete(ConfCxt);

    printf("%d", correct);

    return 0;
}
