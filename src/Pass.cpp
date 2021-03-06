#include "Rllvm.h"

extern "C"
SEXP
R_getPassRegistry()
{
    llvm::PassRegistry *reg = llvm::PassRegistry::getPassRegistry();
    return(R_createRef(reg, "PassRegistry"));
}


extern "C"
SEXP
R_initPassRegistry(SEXP r_registry)
{
    llvm::PassRegistry *Registry;

    if(Rf_length(r_registry))
        Registry = GET_REF(r_registry, PassRegistry);
    else
        Registry = llvm::PassRegistry::getPassRegistry();

    if(!Registry) {
        PROBLEM "no registry"
            ERROR;
    }
    llvm::initializeCore(*Registry);
    llvm::initializeCodeGen(*Registry);
    llvm::initializeLoopStrengthReducePass(*Registry);
    llvm::initializeLowerIntrinsicsPass(*Registry);
#if LLVM_VERSION ==3 && LLVM_MINOR_VERSION < 9
    llvm::initializeUnreachableBlockElimPass(*Registry);
#endif

    return(R_NilValue);
}




#include  <llvm/Transforms/Scalar.h>

#if LLVM_VERSION ==3 && LLVM_MINOR_VERSION >= 5
#undef isNull
#include <llvm/LinkAllPasses.h>
#endif

#define R_CREATE_FUNPASS(id) \
extern "C" \
SEXP  \
R_##id() \
{ \
    llvm::FunctionPass *pass = llvm::id(); \
    return(R_createRef(pass, "FunctionPass")); \
}

#define R_CREATE_PASS(id) \
extern "C" \
SEXP  \
R_##id() \
{ \
    llvm::Pass *pass = llvm::id(); \
    return(R_createRef(pass, "Pass")); \
}

#if LLVM_VERSION <= 3 && LLVM_MINOR_VERSION <= 6
R_CREATE_PASS(createGlobalMergePass)
#else
// In 3.7, this takes 3 arguments - 1 of which is optional.
// const TargetMachine *TM, unsigned MaximalOffset
#pragma message "Note for Rllvm: enable GlobalMergePass for 3.7 and higher"
#endif

R_CREATE_FUNPASS(createAggressiveDCEPass)
R_CREATE_FUNPASS(createDeadCodeEliminationPass)
R_CREATE_FUNPASS(createDeadStoreEliminationPass)
R_CREATE_FUNPASS(createInstructionCombiningPass)
R_CREATE_FUNPASS(createPromoteMemoryToRegisterPass)
R_CREATE_FUNPASS(createDemoteRegisterToMemoryPass)

R_CREATE_PASS(createLICMPass)
R_CREATE_PASS(createSROAPass)
#if LLVM_VERSION ==3 && LLVM_MINOR_VERSION < 9
R_CREATE_PASS(createScalarReplAggregatesPass)
#endif
R_CREATE_PASS(createIndVarSimplifyPass)
R_CREATE_PASS(createLoopStrengthReducePass)
R_CREATE_FUNPASS(createReassociatePass)
R_CREATE_FUNPASS(createCFGSimplificationPass)
R_CREATE_FUNPASS(createJumpThreadingPass)
R_CREATE_FUNPASS(createTailCallEliminationPass)
R_CREATE_FUNPASS(createFlattenCFGPass)

R_CREATE_PASS(createStructurizeCFGPass)
R_CREATE_PASS(createLoopSimplifyPass)
R_CREATE_FUNPASS(createMemCpyOptPass)
#if LLVM_VERSION < 4
R_CREATE_FUNPASS(createCodeGenPreparePass)
#endif
R_CREATE_FUNPASS(createInstructionSimplifierPass)
R_CREATE_FUNPASS(createSinkingPass)

R_CREATE_PASS(createAlwaysInlinerLegacyPass)

R_CREATE_PASS(createLoopDeletionPass)
R_CREATE_PASS(createCorrelatedValuePropagationPass)
R_CREATE_PASS(createPartiallyInlineLibCallsPass)



R_CREATE_PASS(createLoopUnswitchPass)
R_CREATE_PASS(createLoopInstSimplifyPass)
R_CREATE_PASS(createLoopUnrollPass)
R_CREATE_PASS(createLoopRotatePass)
R_CREATE_PASS(createLoopIdiomPass)






#if 0
extern "C"
SEXP 
R_createAggressiveDCEPass()
{
    llvm::FunctionPass *pass = llvm::createAggressiveDCEPass();
    return(R_createRef(pass, "FunctionPass"));
}


extern "C"
SEXP 
R_createDeadCodeEliminationPass()
{
    llvm::FunctionPass *pass = llvm::createDeadCodeEliminationPass();
    return(R_createRef(pass, "FunctionPass"));
}

extern "C"
SEXP 
R_createDeadStoreEliminationPass()
{
    llvm::FunctionPass *pass = llvm::createDeadStoreEliminationPass();
    return(R_createRef(pass, "FunctionPass"));
}


extern "C"
SEXP 
R_createLICMPass()
{
    llvm::FunctionPass *pass = llvm::createLICMPass();
    return(R_createRef(pass, "FunctionPass"));
}
#endif
