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
    }
}

void ParseExpected (Expected* expected, int count) {
    for (int i=0; i<count; ++i) {
        char *type = strtok(NULL, DELIMITER);
        char *value = strtok(NULL, DELIMITER);

        expected[i].type = type;
        expected[i].value = value;
    }
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
    }

    (*correct)++;
}

int main(int argc, char** argv) {
    WasmEdge_ConfigureContext *ConfCxt = WasmEdge_ConfigureCreate();
    WasmEdge_VMContext *VMCxt = WasmEdge_VMCreate(ConfCxt, NULL);

    FILE *fptr = fopen(argv[1], "r");

    char *ptr;
    char buffer[1024];

    unsigned int correct = 0;

    while (fgets(buffer, 1024, fptr)) {
        buffer[strcspn(buffer, "\n")] = 0;

        char *ty = strtok(buffer, DELIMITER);

        if (strcmp(ty, "mo") == 0) {
            char *name = strtok(NULL, DELIMITER);

            WasmEdge_VMLoadWasmFromFile(VMCxt, name);
            WasmEdge_VMValidate(VMCxt);
            WasmEdge_VMInstantiate(VMCxt);
        }

        if (strcmp(ty, "mon") == 0) {
            char *name = strtok(NULL, " ");
            WasmEdge_VMRegisterModuleFromFile(VMCxt, WasmEdge_StringCreateByCString(name), buffer);
        }

        if (strcmp(ty, "ari") == 0) {
            char* fieldStr = strtok(NULL, DELIMITER);
            WasmEdge_String funcName = WasmEdge_StringCreateByCString(fieldStr);

            const WasmEdge_FunctionTypeContext *FuncType =
                WasmEdge_VMGetFunctionType(VMCxt, funcName);

            const int paramCount = WasmEdge_FunctionTypeGetParametersLength(FuncType);
            const int returnCount = WasmEdge_FunctionTypeGetReturnsLength(FuncType);

            WasmEdge_Value Params[paramCount];
            ParseValues(Params, paramCount);

            PrintInvokeMessage(fieldStr, Params, paramCount);

            WasmEdge_Value Returns[returnCount];

            WasmEdge_Result Res = WasmEdge_VMExecute(VMCxt, funcName, Params, paramCount, Returns, returnCount);

            if (WasmEdge_ResultOK(Res)) {
                Expected expected[returnCount];
                ParseExpected(expected, returnCount);

                CheckValues(Returns, expected, returnCount, &correct);
            }

            WasmEdge_StringDelete(funcName);
        }

        if (strcmp(ty, "arim") == 0) {
            WasmEdge_String modName = WasmEdge_StringCreateByCString(strtok(NULL, DELIMITER));

            char* fieldStr = strtok(NULL, DELIMITER);
            WasmEdge_String funcName = WasmEdge_StringCreateByCString(fieldStr);

            const WasmEdge_FunctionTypeContext *FuncType =
                WasmEdge_VMGetFunctionType(VMCxt, funcName);

            const int paramCount = WasmEdge_FunctionTypeGetParametersLength(FuncType);
            const int returnCount = WasmEdge_FunctionTypeGetReturnsLength(FuncType);

            WasmEdge_Value Params[paramCount];
            ParseValues(Params, paramCount);

            PrintInvokeMessage(fieldStr, Params, paramCount);

            WasmEdge_Value Returns[returnCount];

            WasmEdge_Result Res = WasmEdge_VMExecuteRegistered(VMCxt, modName, funcName, Params, paramCount, Returns, returnCount);

            if (WasmEdge_ResultOK(Res)) {
                Expected expected[returnCount];
                ParseExpected(expected, returnCount);

                CheckValues(Returns, expected, returnCount, &correct);
            }

            WasmEdge_StringDelete(modName);
            WasmEdge_StringDelete(funcName);
        }

        if (strcmp(ty, "ati") == 0) {
            char* fieldStr = strtok(NULL, DELIMITER);
            WasmEdge_String funcName = WasmEdge_StringCreateByCString(fieldStr);

            const WasmEdge_FunctionTypeContext *FuncType =
                WasmEdge_VMGetFunctionType(VMCxt, funcName);

            const int paramCount = WasmEdge_FunctionTypeGetParametersLength(FuncType);
            const int returnCount = WasmEdge_FunctionTypeGetReturnsLength(FuncType);

            WasmEdge_Value Params[paramCount];
            ParseValues(Params, paramCount);

            PrintInvokeMessage(fieldStr, Params, paramCount);

            WasmEdge_Value Returns[returnCount];

            WasmEdge_Result Res = WasmEdge_VMExecute(VMCxt, funcName, Params, paramCount, Returns, returnCount);

            if (!WasmEdge_ResultOK(Res)) {
                char* expected = strtok(NULL, DELIMITER);

                if (strcmp(WasmEdge_ResultGetMessage(Res), expected) == 0) {
                    correct += 1;
                }

                else {
                    fprintf(stderr, " Fail!\n Expected: %s\n Actual: %s\n\n", expected, WasmEdge_ResultGetMessage(Res));
                }
            }

            WasmEdge_StringDelete(funcName);
        }
    }

    fclose(fptr);

    WasmEdge_VMDelete(VMCxt);
    WasmEdge_ConfigureDelete(ConfCxt);

    printf("%d", correct);

    return 0;
}
