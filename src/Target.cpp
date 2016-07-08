#include "Rllvm.h"
#include <llvm/Support/TargetRegistry.h>

#if LLVM_VERSION ==3 && LLVM_MINOR_VERSION >= 7
#include <llvm/Analysis/TargetLibraryInfo.h>
#else
#include <llvm/Target/TargetLibraryInfo.h>
#endif

#include <llvm/Target/TargetMachine.h>

// Don't need this - do now - and not in same place on Linux setup.
#ifdef LLVM_DATALAYOUT_H_IN_IR
#include <llvm/IR/DataLayout.h>
#else
#include <llvm/DataLayout.h> 
#endif

extern "C"
SEXP
R_TargetRegistry_lookupTarget(SEXP r_triple, SEXP r_arch)
{
    std::string err;
    const llvm::Target *ans;

    if(Rf_length(r_arch)) {
        std::string arch(CHAR(STRING_ELT(r_arch, 0)));
        llvm::Triple triple(makeTwine(r_triple));
        ans = llvm::TargetRegistry::lookupTarget(arch, triple, err);
    } else {
        std::string triple(CHAR(STRING_ELT(r_triple, 0)));
        ans = llvm::TargetRegistry::lookupTarget(triple, err);
    }

    return(R_createRef(ans, "Target"));
}


#include <llvm/Target/TargetOptions.h>

extern "C"
SEXP
R_Target_createTargetMachine(SEXP r_target, SEXP r_triple, SEXP r_cpu, SEXP r_features, SEXP r_opts)
{
    const llvm::Target *tgt = GET_REF(r_target, Target);
//    llvm::Module *mod = GET_REF(r_module, Module);
    std::string triple(CHAR(STRING_ELT(r_triple, 0)));
    llvm::TargetOptions *opts = NULL, defaultOpts;
    llvm::TargetMachine *ans;

    if(Rf_length(r_opts))
        opts = GET_REF(r_opts, TargetOptions);
    else  {
        /* taken from Halide's CodeGen.cpp */
        defaultOpts.LessPreciseFPMADOption = true;
#ifdef LLV_HAS_NOFRAMEPOINTERELIM
        defaultOpts.NoFramePointerElim = false;
#endif
#ifdef LLVM_HAS_NOFRAMEPOINTERELIMNONLEAF
        defaultOpts.NoFramePointerElimNonLeaf = false;
#endif
        defaultOpts.AllowFPOpFusion = llvm::FPOpFusion::Fast;
        defaultOpts.UnsafeFPMath = true;
        defaultOpts.NoInfsFPMath = true;
        defaultOpts.NoNaNsFPMath = true;
        defaultOpts.HonorSignDependentRoundingFPMathOption = false;
#ifdef LLVM_HAS_USESOFTFLOAT
        defaultOpts.UseSoftFloat = false;
#endif
        defaultOpts.FloatABIType = llvm::FloatABI::Soft;
        defaultOpts.NoZerosInBSS = false;
        defaultOpts.GuaranteedTailCallOpt = false;
#ifdef LLVM_HAS_DISABLETAILCALLS
        defaultOpts.DisableTailCalls = false;
#endif
        defaultOpts.StackAlignmentOverride = 32;
#ifdef LLVM_HAS_REALIGNSTACK
        defaultOpts.RealignStack = true;
#endif
#ifdef LLVM_HAS_TRAPFUNCNAME
        defaultOpts.TrapFuncName = "";
#endif
        defaultOpts.PositionIndependentExecutable = true;

#if LLVM_VERSION == 3 && LLVM_MINOR_VERSION < 5
        defaultOpts.EnableSegmentedStacks = false;
#endif

        defaultOpts.UseInitArray = false;
#ifdef LLVM_HAS_SSPBUFFERSIZE
        defaultOpts.SSPBufferSize = 0;
#endif

        opts = &defaultOpts; 
    }

    ans = tgt->createTargetMachine(triple, std::string(CHAR(STRING_ELT(r_cpu, 0))),
                                   std::string(CHAR(STRING_ELT(r_features, 0))), *opts);
    return(R_createRef(ans, "TargetMachine"));
}

extern "C"
SEXP
R_TargetLibraryInfo_new(SEXP r_triple)
{
//    std::string triple(CHAR(STRING_ELT(r_triple, 0)));
#if LLVM_VERSION == 3 && LLVM_MINOR_VERSION < 8
    llvm::Triple triple(CHAR(STRING_ELT(r_triple, 0)));
    llvm::TargetLibraryInfo *ans = new llvm::TargetLibraryInfo(triple);
    return(R_createRef(ans, "TargetLibraryInfo"));
#else
    PROBLEM "TargetLibraryInfo creationg is not supported in this version of Rllvm"
            ERROR;
                                              
#endif
}




#if !(LLVM_VERSION == 3 && LLVM_MINOR_VERSION > 5)
extern "C"
SEXP
R_TargetMachine_getDataLayout(SEXP r_tm)
{
    llvm::TargetMachine *tm = GET_REF(r_tm, TargetMachine);
    const llvm::DataLayout *ans = tm->getDataLayout();
    return(R_createRef(ans, "DataLayout"));
}
#endif

extern "C"
SEXP
R_DataLayout_getStringRepresentation(SEXP r_dl)
{
    llvm::DataLayout *dl = GET_REF(r_dl, DataLayout);
    std::string str = dl->getStringRepresentation();
    return(str.data() ? mkString(str.data()) : R_NaString);
}



extern "C"
SEXP
R_TargetMachine_addAnalysisPasses(SEXP r_targetMachine, SEXP r_passManager)
{
#if  LLVM_VERSION==3 && LLVM_MINOR_VERSION < 7
    llvm::TargetMachine *targetMachine = GET_REF(r_targetMachine, TargetMachine);
    llvm::legacy::PassManager *passManager = GET_REF(r_passManager, legacy::PassManager);
    targetMachine->addAnalysisPasses(*passManager);
#else
    PROBLEM "addAnalysisPasses removed from LLVM after 3.6"
        ERROR;
#endif
    return(R_NilValue);
}

extern "C"
SEXP
R_TargetMachine_addPassesToEmitFile(SEXP r_targetMachine, SEXP r_passManager, SEXP r_out, SEXP r_fileType)
{
#if  LLVM_VERSION==3 && LLVM_MINOR_VERSION < 7
    llvm::TargetMachine *targetMachine = GET_REF(r_targetMachine, TargetMachine);
    llvm::legacy::PassManager *passManager = GET_REF(r_passManager, legacy::PassManager);
    llvm::formatted_raw_ostream *out;
    out = GET_REF(r_out, formatted_raw_ostream);

    // passManager is now a legacy::PassManager not a PassManagerBase
    bool ans = targetMachine->addPassesToEmitFile(*passManager, *out, (llvm::TargetMachine::CodeGenFileType) INTEGER(r_fileType)[0]);
    /* ans is true if addPasses... failed */
    return(ScalarLogical(ans == true));
#else
    PROBLEM "addPassesToEmitFile does not yet work with the legacy::PassManager"
        ERROR;
#endif
}

extern "C"
SEXP
R_Target_getName(SEXP r_target)
{
    llvm::Target *target = GET_REF(r_target, Target);
    std::string str = target->getName();
    return(mkString(str.data()));
}




/* Show the registered targets. One has to initialize them first.
   InitializeNVPTXTarget()
   InitializeCppBackendTarget()
 */
extern "C"
SEXP
R_printRegisteredTargetsForVersion()
{
    llvm::TargetRegistry::printRegisteredTargetsForVersion();
    
    return(R_NilValue);
}


#include <llvm/Support/TargetSelect.h>


#define MAKE_InitializeAll(suffix) \
 extern "C" \
 SEXP \
 R_InitializeAll##suffix() \
 { \
     llvm::InitializeAll ## suffix (); \
     return(R_NilValue); \
 } 

MAKE_InitializeAll(TargetInfos)
MAKE_InitializeAll(Targets)
MAKE_InitializeAll(TargetMCs)
MAKE_InitializeAll(AsmPrinters)
MAKE_InitializeAll(AsmParsers)
MAKE_InitializeAll(Disassemblers)





#include <llvm/Support/Host.h>
extern "C"
SEXP
R_getDefaultTargetTriple()
{
    std::string tri = llvm::sys::getDefaultTargetTriple();
    return(mkString(tri.c_str()));
}
