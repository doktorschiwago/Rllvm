#include "Rllvm.h"
#if 0
#include <llvm/IR/Attributes.h>
#endif


extern "C"
SEXP
R_Function_doesNotThrow(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    return(ScalarLogical(func->doesNotThrow()));
}

extern "C"
SEXP
R_Function_setDoesNotThrow(SEXP r_func, SEXP r_val)
{
    llvm::Function *func = GET_REF(r_func, Function);
#ifndef LLVM_VERSION_THREE_TWO
    func->setDoesNotThrow(LOGICAL(r_val)[0]);
#else
    func->setDoesNotThrow();
#endif
    return(R_NilValue);
}


extern "C"
SEXP
R_Function_onlyReadsMemory(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    return(ScalarLogical(func->onlyReadsMemory()));
}




extern "C"
SEXP
R_Function_setOnlyReadsMemory(SEXP r_func, SEXP r_val)
{
    llvm::Function *func = GET_REF(r_func, Function);
#ifndef LLVM_VERSION_THREE_TWO
    func->setOnlyReadsMemory(LOGICAL(r_val)[0]);
#else
    func->setOnlyReadsMemory();
#endif
    return(R_NilValue);
}

extern "C"
SEXP
R_Function_doesNotAccessMemory(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    return(ScalarLogical(func->doesNotAccessMemory()));
}

extern "C"
SEXP
R_Function_setDoesNotAccessMemory(SEXP r_func, SEXP r_val)
{
    llvm::Function *func = GET_REF(r_func, Function);
#ifndef LLVM_VERSION_THREE_TWO
    func->setDoesNotAccessMemory(LOGICAL(r_val)[0]);
#else
    func->setDoesNotAccessMemory();
#endif
    return(R_NilValue);
}


extern "C"
SEXP
R_Function_getGC(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    const char *str;
#if (LLVM_VERSION == 3 && LLVM_MINOR_VERSION >= 8) || LLVM_VERSION >= 4
    str = func->getGC().c_str();
#else
    str = func->getGC();  //XXX Define for 3.5 and check others. 3.8 returns a std::string
#endif
    if(str)
        return(ScalarString(mkChar(str)));
    else
        return(NEW_CHARACTER(0));
}

extern "C"
SEXP
R_Function_hasGC(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    return(ScalarLogical(func->hasGC()));
}


extern "C"
SEXP
R_Function_getCallingConv(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    return(ScalarInteger(func->getCallingConv()));
}

extern "C"
SEXP
R_Function_setCallingConv(SEXP r_func, SEXP r_val)
{
    llvm::Function *func = GET_REF(r_func, Function);
    func->setCallingConv((llvm::CallingConv::ID) INTEGER(r_val)[0]);
    return(R_NilValue);
}


extern "C"
SEXP
R_Function_getBasicBlockList(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    int n, i = 0;
    SEXP rans, names;

#if LLVM_VERSION == 3 && LLVM_MINOR_VERSION < 8
    llvm::iplist<llvm::BasicBlock> &blocks = func->getBasicBlockList();
#else
    llvm::Function::BasicBlockListType &blocks = func->getBasicBlockList();
#endif

    n = blocks.size();

    PROTECT(rans = NEW_LIST(n));
    PROTECT(names = NEW_CHARACTER(n));
#if 1
#if LLVM_VERSION == 3 && LLVM_MINOR_VERSION < 8
    for(llvm::iplist<const llvm::BasicBlock>::const_iterator it = blocks.begin(); it != blocks.end(); it++, i++)
#else
    for(llvm::Function::BasicBlockListType::const_iterator it = blocks.begin(); it != blocks.end(); it++, i++)
#endif
    {
        const llvm::BasicBlock *cur = &(*it);
//        SET_STRING_ELT(names, i, mkChar(cur->getNameStr().data())); // Worked for llvm 2.8
        SET_STRING_ELT(names, i, mkChar(cur->getName().data()));
        SET_VECTOR_ELT(rans, i, R_createRef(cur, "BasicBlock"));
    }
#endif
    SET_NAMES(rans, names);

    UNPROTECT(2);
    return(rans);
}


extern "C"
SEXP
R_Function_getReturnType(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    return(R_createRef(func->getReturnType(), "Type"));
}



extern "C"
SEXP
R_Function_getParam(SEXP r_func,  SEXP r_whichParam)
{
     llvm::Function *func = GET_REF(r_func, Function);
     unsigned i, num = INTEGER(r_whichParam)[0];
     llvm::Function::ArgumentListType &args = func->getArgumentList();
     if(num >= args.size()) {
         PROBLEM "invalid parameter index, only %d parameters", (int) args.size()
             ERROR;
     }

// llvm::Argument *a = args[num];

     llvm::Function::ArgumentListType::iterator arg = args.begin();

     for(i = 0 ; i < num; i++, arg++) {  }
        llvm::Argument *el;
        el = &(*arg); //XXX 3.8 // Don't use arg.getNodePtrUnchecked() as that is internal, but leave it here for a note.
     return(R_createRef(el, "Argument"));
}


extern "C"
SEXP
R_Function_getContext(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    llvm::LLVMContext *ctxt;
    ctxt = &(func->getContext());
    return(R_createRef(ctxt, "LLVMContext"));
}

extern "C"
SEXP
R_setFunctionParamNames(SEXP r_func, SEXP r_names)
{
     llvm::Function *func = GET_REF(r_func, Function);
     llvm::Function::ArgumentListType &args = func->getArgumentList();
     int num = args.size();

     if(Rf_length(r_names) < num) {
         PROBLEM "fewer names provided than parameters for function %s (%d versus %d)", func->getName().data(), Rf_length(r_names), num
	    WARN;
         num = Rf_length(r_names);
      }

     llvm::Function::ArgumentListType::iterator arg = args.begin();

     for(int i = 0 ; i < num; i++, arg++) {  
         arg->setName( CHAR(STRING_ELT(r_names, i)));
     }

     return(r_func);
}


// See http://llvm.org/docs/ReleaseNotes.html
#ifdef NEW_LLVM_ATTRIBUTES_SETUP
typedef llvm::Attribute::AttrKind AttrKind;
#else
typedef llvm::Attributes::AttrVal AttrKind;
#endif

SEXP
R_Argument_setAttrs(llvm::Argument *arg, SEXP r_vals, llvm::LLVMContext *ctxt)
{
        /* now have the parameter, so set the values. */
  llvm::AttrBuilder builder;
  for(unsigned i = 0 ; i < (unsigned) Rf_length(r_vals); i++)  
      builder.addAttribute( (AttrKind) INTEGER(r_vals)[i] );


#ifdef  NEW_LLVM_ATTRIBUTES_SETUP
  /* The 1 here appears to be the position/index of the argument. They start at 1 and move up. 
     We need to know which one it is. Same for getting the string below for the return.
     Shall we determine the index in this routine or require the caller to specify it.
   */
  llvm::AttributeSet attrs = llvm::AttributeSet::get(*ctxt, 1/*!!!!*/, builder);
#else
  llvm::Attributes attrs;
  attrs = llvm::Attributes::get(llvm::getGlobalContext() , builder);
#endif
  arg->addAttr(attrs);

#ifdef  NEW_LLVM_ATTRIBUTES_SETUP
  std::string str = attrs.getAsString(0);          /*XXX WHat should the Idx argument be? */
  return(str.data() ? mkString(str.data()) : R_NaString);
#else
  return(ScalarString(mkChar(attrs.getAsString().data())));
#endif
}



/* Kill this off now that we can get the Argument back. */
#if 0  // XXX 3.8  see above - wrong signature now.
extern "C"
SEXP
R_Function_setParamAttributes(SEXP r_func,  SEXP r_whichParam, SEXP r_vals)
{
     llvm::Function *func = GET_REF(r_func, Function);
     unsigned i, num = INTEGER(r_whichParam)[0];
     llvm::Function::ArgumentListType &args = func->getArgumentList();

     if(num >= args.size()) {
         PROBLEM "incorrect parameter number, should be less than %d", (int)args.size()
             ERROR;
     }

     llvm::Function::ArgumentListType::iterator arg = args.begin();
     for(i = 0 ; i < num; i++, arg++) {  }

     return(R_Argument_setAttrs(arg, r_vals));
}
#endif

extern "C"
SEXP
R_Argument_setAttributes(SEXP r_arg, SEXP r_vals, SEXP r_context)
{
     llvm::Argument *arg = GET_REF(r_arg, Argument);
     llvm::LLVMContext *ctxt = GET_REF(r_context, LLVMContext);

     if(!ctxt)
#if LLVM_VERSION == 3 && LLVM_MINOR_VERSION < 9
         ctxt = &llvm::getGlobalContext();
#else
         ctxt = &(arg->getParent()->getContext());
#endif

     return(R_Argument_setAttrs(arg, r_vals, ctxt));
}



extern "C"
SEXP
R_Argument_getParent(SEXP r_arg)
{ 
     llvm::Argument *arg = GET_REF(r_arg, Argument);
     llvm::Function *ans;
     ans = arg->getParent();
     return(R_createRef(ans, "Function"));    
}

extern "C"
SEXP
R_Argument_mutateType(SEXP r_arg, SEXP r_type)
{
     llvm::Argument *arg = GET_REF(r_arg, Argument);
     llvm::Type *ty = GET_TYPE(r_type);
     arg->mutateType(ty);
     return(R_NilValue);
}



#define R_LLVM_ARG_HAS(name) \
extern "C" \
SEXP  \
R_Argument_has##name##Attr(SEXP r_arg, SEXP r_vals) \
{ \
     llvm::Argument *arg = GET_REF(r_arg, Argument); \
     return(ScalarLogical(arg->has##name##Attr())); \
}


R_LLVM_ARG_HAS(ByVal)
R_LLVM_ARG_HAS(Nest)
R_LLVM_ARG_HAS(NoAlias)
R_LLVM_ARG_HAS(NoCapture)
R_LLVM_ARG_HAS(StructRet)
//R_LLVM_ARG_HAS(InAlloca)


extern "C"
SEXP
R_Argument_onlyReadsMemory(SEXP r_func)
{
    llvm::Argument *func = GET_REF(r_func, Argument);
    return(ScalarLogical(func->onlyReadsMemory()));
}

#if 0
extern "C"
SEXP
R_Argument_hasByValAttr(SEXP r_arg, SEXP r_vals)
{
     llvm::Argument *arg = GET_REF(r_arg, Argument);
     return(ScalarLogical(arg->hasByValAttr()));
}
#endif



extern "C"
SEXP
R_Function_setAttributes(SEXP r_func, SEXP r_vals)
{
     llvm::Function *func = GET_REF(r_func, Function);
     for(int i = 0; i < Rf_length(r_vals); i++) {
         func->addFnAttr( (AttrKind)  INTEGER(r_vals)[i]);
     }
     return(ScalarLogical(TRUE));

}

extern "C"
SEXP
R_Function_removeAttributes(SEXP r_func, SEXP r_vals)
{
     llvm::Function *func = GET_REF(r_func, Function);
     for(int i = 0; i < Rf_length(r_vals); i++) {
         func->removeFnAttr( (AttrKind)  INTEGER(r_vals)[i]);
     }
     return(ScalarLogical(TRUE));

}

extern "C"
SEXP
R_Function_getAttributes(SEXP r_func)
{
     llvm::Function *func = GET_REF(r_func, Function);

#ifdef  NEW_LLVM_ATTRIBUTES_SETUP
     llvm::AttributeSet attrs;
     attrs = func->getAttributes();
     return(R_getFunctionAttributes_logical(attrs));
#else
     // have to assign in declaration's initialization.
     const llvm::AttrListPtr attrs = func->getAttributes();
     llvm::Attributes fnattrs = attrs.getFnAttributes();
     return(R_getFunctionAttributes_logical(fnattrs));
#endif
}



extern "C"
SEXP
R_Function_isVarArg(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    return(ScalarLogical(func->isVarArg()));
}



extern "C"
SEXP
R_Function_hasMetadata(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    return(ScalarLogical(func->hasMetadata()));
}


extern "C"
SEXP
R_Function_getMetadata(SEXP r_func, SEXP r_kind)
{
    llvm::Function *func = GET_REF(r_func, Function);
    llvm::MDNode *node;
    if(TYPEOF(r_kind) == STRSXP) 
        node = func->getMetadata(llvm::StringRef(CHAR(STRING_ELT(r_kind, 0))));
    else
        node = func->getMetadata((unsigned) INTEGER(r_kind)[0]);

    if (node) {
        return(R_createRef(node, "MDNode"));
    } else {
        return(R_NilValue);
    }
}

extern "C"
SEXP
R_Function_setMetadata(SEXP r_func, SEXP r_kind, SEXP r_node)
{
    llvm::Function *func = GET_REF(r_func, Function);
    llvm::MDNode *node = GET_REF(r_node, MDNode);
    if(TYPEOF(r_kind) == STRSXP) 
        func->setMetadata(llvm::StringRef(CHAR(STRING_ELT(r_kind, 0))), node);
    else
        func->setMetadata((unsigned) INTEGER(r_kind)[0], node);

    return(R_NilValue);
}

// class is used by Function_clone
class IdentityMapper: public llvm::ValueToValueMapTy
{
public:
    llvm::Type *remapType(llvm::Type *SrcTy)
    {
        return SrcTy;
    }
};

extern "C"
SEXP
R_Function_clone(SEXP r_func, SEXP r_newName)
{
    llvm::Function *func = GET_REF(r_func, Function);

    llvm::Module *mod = func->getParent();

	IdentityMapper VMap;
    llvm::ClonedCodeInfo cci;

  	//llvm::Function* duplicateFunction = NULL;
    llvm::Constant* duplicateFunction = mod->getOrInsertFunction(CHAR(STRING_ELT(r_newName, 0)), func->getFunctionType(),
        func->getAttributes());

    if (! llvm::isa<llvm::Function>(duplicateFunction)) return(R_NilValue);

    llvm::Function* duplicateFunction2=llvm::cast<llvm::Function>(duplicateFunction);

   std::vector<llvm::Type*> ArgTypes;
   
     // The user might be deleting arguments to the function by specifying them in
     // the VMap.  If so, we need to not add the arguments to the arg ty vector
     //
     for (const llvm::Argument &I : func->args())
       if (VMap.count(&I) == 0) // Haven't mapped the argument to anything yet?
         ArgTypes.push_back(I.getType());
   
   
     // Loop over the arguments, copying the names of the mapped arguments over...
     llvm::Function::arg_iterator DestI = duplicateFunction2->arg_begin();
     for (const llvm::Argument & I : func->args())
       if (VMap.count(&I) == 0) {     // Is this argument preserved?
         DestI->setName(I.getName()); // Copy the name over...
         VMap[&I] = &*DestI++;        // Add mapping to VMap
       }
   
     llvm::SmallVector<llvm::ReturnInst*, 8> Returns;  // Ignore returns cloned.
     llvm::CloneFunctionInto(duplicateFunction2, func, VMap, func->getSubprogram() != nullptr, Returns, "",
                       &cci);
    
    //func->getParent()->getFunctionList().push_back(duplicateFunction);

    return(R_createRef(duplicateFunction2, "Function"));
}

extern "C"  
SEXP
R_Function_dominates(SEXP r_func, SEXP r_instruction1, SEXP r_instruction2)
{
    llvm::Function *func = GET_REF(r_func, Function);    

    llvm::legacy::FunctionPassManager mgr(func->getParent());

    llvm::DominatorTreeWrapperPass* DTW=new llvm::DominatorTreeWrapperPass();

    mgr.add(DTW);

    mgr.run(*func);

    DTW->runOnFunction(*func);

    const llvm::DominatorTree &DT=DTW->getDomTree();

    llvm::Instruction *ins1 = GET_REF(r_instruction1, Instruction);
    llvm::Instruction *ins2 = GET_REF(r_instruction2, Instruction);

    DT.verifyDomTree();

    bool res=DT.dominates(ins1, ins2);


    return(ScalarLogical(res));
}


extern "C"
SEXP
R_Function_postdominates(SEXP r_func, SEXP r_block1, SEXP r_block2)
{
    llvm::Function *func = GET_REF(r_func, Function);    

    llvm::legacy::FunctionPassManager mgr(func->getParent());

    llvm::PostDominatorTreeWrapperPass* DTW=new llvm::PostDominatorTreeWrapperPass();

    mgr.add(DTW);

    mgr.run(*func);

    DTW->runOnFunction(*func);

    const llvm::PostDominatorTree &DT=DTW->getPostDomTree();

    llvm::BasicBlock *ins1 = GET_REF(r_block1, BasicBlock);
    llvm::BasicBlock *ins2 = GET_REF(r_block2, BasicBlock);

    //DT.verify();

    bool res=DT.dominates(ins1, ins2);


    return(ScalarLogical(res));
}



extern "C"
SEXP
R_Argument_cast(SEXP r_var)
{
    llvm::Value *var = GET_REF(r_var, Value);

	int ans = 0;

	ans = llvm::Argument::classof(var);

	if (ans) {
        
		return(R_createRef(var, "Argument"));
	} else {
    		return(ScalarLogical(FALSE));
	}
}

extern "C"
SEXP
R_Function_getSubprogram(SEXP r_func)
{
    llvm::Function *func = GET_REF(r_func, Function);
    llvm::DISubprogram *node;
    node = func->getSubprogram();

    if (node) {
        return(R_createRef(node, "DISubprogram"));
    } else {
        return(R_NilValue);
    }
}

