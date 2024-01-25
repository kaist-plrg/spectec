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
        default:
            break;
    }
}

void PrintInvokeMessage (const char* funcName, WasmEdge_Value* Params, int paramCount) {
    fprintf(stderr, "[Invoking %s", funcName);

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
        if (strcmp(type, "funcref") == 0) {
            Values[i] = (strcmp(value, "null") == 0)
            ? WasmEdge_ValueGenFuncRef(NULL)
            : WasmEdge_ValueGenFuncRef((void*)binary.i64 + 0x100000000ULL);
        }
        if (strcmp(type, "externref") == 0)
            Values[i] = (strcmp(value, "null") == 0)
            ? WasmEdge_ValueGenExternRef(NULL)
            : WasmEdge_ValueGenExternRef((void*)binary.i64 + 0x100000000ULL);
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

bool CheckText (WasmEdge_Result Res, char* expected, unsigned int* correct) {
    if (!WasmEdge_ResultOK(Res)) {
        if (strcmp(WasmEdge_ResultGetMessage(Res), expected) == 0) {
            (*correct)++;
        }

        else {
            fprintf(stderr, " Fail!\n Expected: %s\n Actual: %s\n\n", expected, WasmEdge_ResultGetMessage(Res));
            return false;
        }
    }

    return true;
}

void CheckValues (WasmEdge_Value* actual, Expected* expected, const size_t count, unsigned int* correct) {
    for (int i=0; i<count; ++i) {
        union Input binary = { strtoull(expected[i].value, NULL, 10) };

        if (strcmp(expected[i].type, "i32") == 0 && (actual[i].Type != WasmEdge_ValType_I32 || binary.i32 != WasmEdge_ValueGetI32(actual[i]))) {
            return PrintFailAssertReturn(actual, expected, count);
        }

        if (strcmp(expected[i].type, "i64") == 0 && (actual[i].Type != WasmEdge_ValType_I64 || binary.i64 != WasmEdge_ValueGetI64(actual[i]))) {
            return PrintFailAssertReturn(actual, expected, count);
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
                return PrintFailAssertReturn(actual, expected, count);
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
                return PrintFailAssertReturn(actual, expected, count);
        }

        if (strcmp(expected[i].type, "funcref") == 0) {
            if (actual[i].Type == WasmEdge_ValType_FuncRef) {
                if (strcmp(expected[i].value, "null") == 0 && WasmEdge_ValueGetFuncRef(actual[i]) == NULL) {
                    continue;
                }
            }

            return PrintFailAssertReturn(actual, expected, count);
        }

        if (strcmp(expected[i].type, "externref") == 0) {
            if (actual[i].Type == WasmEdge_ValType_ExternRef) {
                if (strcmp(expected[i].value, "null") == 0 && WasmEdge_ValueGetExternRef(actual[i]) == NULL) {
                    continue;
                }
            }

            return PrintFailAssertReturn(actual, expected, count);
        }
    }

    (*correct)++;
}

InvokeResult Invoke (WasmEdge_VMContext *VMCtx) {
    WasmEdge_String modName = WasmEdge_StringCreateByCString(strtok(NULL, DELIMITER));

    char* fieldStr = strtok(NULL, DELIMITER);
    WasmEdge_String funcName = WasmEdge_StringCreateByCString(fieldStr);

    const WasmEdge_FunctionTypeContext *FuncType =
        WasmEdge_VMGetFunctionTypeRegistered(VMCtx, modName, funcName);

    const size_t paramCount = WasmEdge_FunctionTypeGetParametersLength(FuncType);
    const size_t returnCount = WasmEdge_FunctionTypeGetReturnsLength(FuncType);

    WasmEdge_Value Params[paramCount];
    ParseValues(Params, paramCount);

    PrintInvokeMessage(fieldStr, Params, paramCount);

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

                CheckValues(result.Returns, expected, result.returnCount, &correct);
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

                CheckValues(actual, expected, 1, &correct);
            }
        }

        if (strcmp(ty, "ati") == 0) {
            InvokeResult result = Invoke(VMCtx);

            if (!WasmEdge_ResultOK(result.Res)) {
                char* expected = strtok(NULL, DELIMITER);

                if (strcmp(WasmEdge_ResultGetMessage(result.Res), expected) == 0) {
                    correct += 1;
                }

                else {
                    fprintf(stderr, " Fail!\n Expected: %s\n Actual: %s\n\n", expected, WasmEdge_ResultGetMessage(result.Res));
                }
            }
        }

        if (strcmp(ty, "aiv") == 0) {
            char* path = strtok(NULL, DELIMITER);
            char* expected = strtok(NULL, DELIMITER);

            WasmEdge_Result Res = WasmEdge_VMLoadWasmFromFile(VMCtx, path);
            if (!CheckText (Res, expected, &correct))
                continue;

            Res = WasmEdge_VMValidate(VMCtx);
            if (!CheckText (Res, expected, &correct))
                continue;
        }

        if (strcmp(ty, "aun") == 0) {
            char* path = strtok(NULL, DELIMITER);
            char* expected = strtok(NULL, DELIMITER);

            WasmEdge_Result Res = WasmEdge_VMLoadWasmFromFile(VMCtx, path);
            if (!CheckText (Res, expected, &correct))
                continue;

            Res = WasmEdge_VMValidate(VMCtx);
            if (!CheckText (Res, expected, &correct))
                continue;

            Res = WasmEdge_VMInstantiate(VMCtx);
            if (!CheckText (Res, expected, &correct))
                continue;
        }
    }

    fclose(fptr);

    WasmEdge_VMDelete(VMCtx);
    WasmEdge_ConfigureDelete(ConfCxt);

    printf("%d", correct);

    return 0;
}