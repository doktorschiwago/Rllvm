mdString =
function(str, ctx = getGlobalContext())  
{
  .Call("R_MDString_get", as(ctx, "LLVMContext"), as.character(str))
}


setMethod("setMetadata",
          c("Module", "character"),
          function(x, id, values, context = getContext(x), ...) {
             setMetadata(getMetadata(x, id, create = TRUE), values = values, context = context, ...)
          })

setMethod("setMetadata",
          c("NamedMDNode"),
          function(x, id, values, context = getContext(x), ...) {
            if(!is.list(values))
              values = list(values)
            
            w = base::sapply(values, is, "Value")
            if(!all(w)) 
              values[!w] = lapply(values[!w], makeMetadataValue, context = context)

            .Call("R_NamedMDNode_addOperand1", x, values, context)
          })




setMethod("getMetadata", c("Module", "character"),
          function(obj, id, create = FALSE)
            if(create)
               .Call("R_getOrInsertNamedMetadata", obj, as.character(id))
            else
               .Call("R_Module_getNamedMetadata", obj, as.character(id)))


setMethod("getMetadata", c("Module", "missing"),
          function(obj, id)
            .Call("R_Module_getNamedMDList", obj))



setGeneric("makeMetadataValue",
           function(value, context = getGlobalContext(), ...)
             standardGeneric("makeMetadataValue"))

setMethod("makeMetadataValue",
          "integer",
           function(value, context = getGlobalContext(), ...)
             createIntegerConstant(value, context))

setMethod("makeMetadataValue",
          "numeric",
           function(value, context = getGlobalContext(), ...)
             createFloatingPointConstant(value, context))

setMethod("makeMetadataValue",
          "character",
           function(value, context = getGlobalContext(), ...)
            mdString(value, context))


setMethod("getOperands", "NamedMDNode",
           function(x, ...)
            coerceGenericMetadata(.Call("R_NamedMDNode_getOperands", x)))

setMethod("getOperands", "MDNode",
           function(x, ...)
            coerceGenericMetadata(.Call("R_MDNode_getOperands", x)))


setMethod("getName", "NamedMDNode",
           function(obj, ...)
            .Call("R_NamedMDNode_getName", obj))

setMethod("getNumOperands", "NamedMDNode",
           function(x, ...)
            .Call("R_NamedMDNode_getNumOperands", x))

setMethod("getNumOperands", "MDNode",
           function(x, ...)
            .Call("R_MDNode_getNumOperands", x))


# Make these lighter-weight by not getting all the operands.
# C++ code
setMethod("length", "NamedMDNode",
          function(x)
           length(getOperands(x)))

setMethod("[[", c("NamedMDNode", "numeric"),
           function(x, i, j, ...) {
             getOperands(x)[[i]]
           })
setMethod("[", c("NamedMDNode", "numeric"),
           function(x, i, j, ...) {
             getOperands(x)[i]
           })

setMethod("[", c("NamedMDNode", "missing"),
           function(x, i, j, ...) {
             getOperands(x)[]
           })


setMethod("length", "MDNode",
          function(x)
           length(getOperands(x)))

setMethod("[[", c("MDNode", "numeric"),
           function(x, i, j, ...) {
             getOperands(x)[[i]]
           })
setMethod("[", c("MDNode", "numeric"),
           function(x, i, j, ...) {
             getOperands(x)[i]
           })

setMethod("[", c("MDNode", "missing"),
           function(x, i, j, ...) {
             getOperands(x)[]
           })




setMethod("getParent", "NamedMDNode",
          function(x) {
              .Call("R_NamedMDNode_getParent", x)
          })

getMetadataKind = function(md) {
    return(.Call("R_Metadata_getMetadataID", as(md, "Metadata")))
}

MetadataKindClass=as.list(rep(NA, times=length(MetadataKind)))

MetadataKindClass[[DIBasicTypeKind]]="DIType"
MetadataKindClass[[DIDerivedTypeKind]]="DIType"
MetadataKindClass[[DICompositeTypeKind]]="DIType"
MetadataKindClass[[DISubroutineTypeKind]]="DISubroutineType"
MetadataKindClass[[DICompileUnitKind]]="DICompileUnit"
MetadataKindClass[[DISubprogramKind]]="DISubprogram"
MetadataKindClass[[MDTupleKind]]="MDNode"


coerceGenericMetadata =
function(ins)
{
    #browser()
  	if(is.list(ins))
    	return(lapply(ins, coerceGenericMetadata))

    if (is.null(ins)) return(ins)

  	type = getMetadataKind(ins)
    if (type == 0) return(ins)
  	k = MetadataKindClass[[type]]
  	if(is.na(k)) {
		return(ins)
  	}
  
  	as(ins, k)
}

