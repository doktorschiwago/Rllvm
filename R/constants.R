setAs("IRBuilder", "LLVMContext",
         function(from) 
           getGlobalContext())  # should try to get it from any of the blocks.

createConstant = makeConstant =
function(builder, val, type = NULL,
           context = if(!missing(builder)) 
                        as(builder, "LLVMContext") 
                     else
                        getGlobalContext())
{

  if(!is.null(type)) {

       if(sameType(type, Int32Type))
           val = as.integer(val)
       else if(sameType(type, DoubleType))
           val = as.numeric(val)
       else if(sameType(type, StringType))
           val = as.character(val)                
  }
    
  if(is.logical(val)) {
    return(createLogicalConstant(val, context))
  } else if(is.integer(val)) {
    return(createIntegerConstant(val, context, type))
  } else if(is.numeric(val)) {
    return(createFloatingPointConstant(val, context, type))
  } else if(is.character(val)) {
     return(createStringConstant(val, context, type))
  } else
    stop("Don't know yet how to create such a constant")
}

createLogicalConstant =
function(val, context = getGlobalContext())
{
   .Call("R_getLogicalConstant", as.logical(val), context)
}

setGeneric("createIntegerConstant",
            function(val, context = getGlobalContext(), ...)
               standardGeneric("createIntegerConstant"))

setMethod("createIntegerConstant",
           c("character"), 
function(val, context = getGlobalContext(), type = NULL, bitwidth = 32L, radix = 10L, ...)
{
   .Call("R_createIntegerConstant_from_string", val, context, as.integer(bitwidth), as.integer(radix))
})


.tmp = function(val, context = getGlobalContext(), type = NULL, bitwidth = 32L, radix = 10L, ...)
{
     # if the caller didn't specify the bitwidth but the type, use the type's bitwidth.
  if(!is.null(type)) {
     bw = getIntegerBitWidth(type)
     if(missing(bitwidth))
        bitwidth = bw
     else if(bitwidth != bw)
         stop("diffferent integer bitwidths")
  }
   createIntegerConstant(as.character(val), context, type, bitwidth, radix)
}
setMethod("createIntegerConstant", c("integer"), .tmp)
setMethod("createIntegerConstant", c("numeric"), .tmp)

rm(.tmp)

createFloatingPointConstant = createDoubleConstant =
function(val, context = getGlobalContext(), type = DoubleType)
{
  .Call("R_createFPConstant", as.numeric(val), context, type)
}


createStringConstant =
function(val, context = getGlobalContext(), type = NULL)
{
  .Call("R_createStringConstant", as.character(val), context, type)
}


getGetElementPtr =
function(cons, index = c(0L, 0L), inBounds = FALSE, ctx = getContext(cons))
{
   index = lapply(index, function(x) if(!is(x, "Constant")) createConstant(val = x, context = ctx) else x)
   .Call("R_Constant_getGetElementPtr", cons, index, as.logical(inBounds))
}


constantAggregateZero =
function(type)
{
   .Call("R_ConstantAggregateZero_get", as(type, "Type"))
}



setMethod("getValue", "ConstantInt",
           function(x, ...)
              .Call("R_ConstantInt_getValue", x))

setMethod("getValue", "ConstantFP",
           function(x, ...)
              .Call("R_ConstantFP_getValue", x))


getNULLPointer = # getNULLPointerType =
function(type)
{
  .Call("R_ConstantPointerNull_get", type)
}

constantCast =
function(var)
{


    if(!is(var, "Value"))
        stop("Need a Value")

    ans=.Call("R_Constant_cast", var)
    
    return(ans)

}


constantInstructionCast =
function(var)
{


    if(!is(var, "Constant"))
        stop("Need a Constant")

    ans=.Call("R_ConstantInstruction_cast", var)

    if (is.logical(ans)) return(ans)
    
    return(coerceGenericInstruction(ans))

}
