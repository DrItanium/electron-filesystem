#ifndef binary_operations_h
#define binary_operations_h

extern void BinaryOperationsFunctionDefinitions(void *theEnv);
extern long long RightShiftFunction(void*);
extern long long LeftShiftFunction(void*);
extern long long BinaryAndFunction(void*);
extern long long BinaryOrFunction(void*);
extern long long BinaryXorFunction(void*);
extern long long BinaryNotFunction(void*);
extern long long SliceFunction(void*);
extern void Slice8Function(void*, DATA_OBJECT_PTR);
extern void Slice4Function(void*, DATA_OBJECT_PTR);
extern void Slice2Function(void*, DATA_OBJECT_PTR);
extern long long MergeFunction(void*);
#endif
