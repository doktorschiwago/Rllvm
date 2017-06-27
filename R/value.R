setAs("GlobalValue", "Module",
       function(from)
         .Call("R_GlobalValue_getParent", from))


setGeneric("getType",
            function(obj, ...)
              standardGeneric("getType"))

setMethod("getType", "Value",
           function(obj, ...) {
              .Call("R_Value_getType", obj)
        })



getLinkage =
function(obj)
{
   ans = .Call("R_GlobalValue_getLinkage", obj)
   # now match to known constants
   i = match(ans, LinkageTypes)
   LinkageTypes[i]
}



replaceAllUsesWith =
function(val, other)
  .Call("R_Value_replaceAllUsesWith", as(val, "Value"), as(other, "Value"))    

getAllUses = 
function(obj)
    .Call("R_Value_getAllUses", as(obj, "Value"))


# Sorry, I dont get the difference between Use and User and how all this adds up
# This functions return instructions that depend on the value
getAllValueUsers = 
function(obj) {
    uses=coerceGenericInstruction(.Call("R_Value_getAllUsers", as(obj, "Value")), throwError=FALSE)

    if (length(uses)<1) return(uses)

    if (length(uses)==1) {
        attr(uses[[1]], "llvmDominates")=TRUE
        return(uses)
    }

    for (i in 1:length(uses)) {
            attr(uses[[i]], "llvmDominates")=FALSE
    }

    #check if obj is a function

    if (is(obj, "Argument")) {
        func=getParent(obj)
    } else {
        bb=getParent(obj)

        if (! is(bb, "BasicBlock")) return(uses)

        func=getParent(bb)

        if (! is(func, "Function")) return(uses)
    }

    #order uses by dominance

    #-browser()

    index=1

    while (TRUE) {

        change=FALSE

        for (i in index:(length(uses))) {
            dominatesAll=TRUE
            for (j in index:length(uses)) {
                if (i==j) next
                if (! dominates(func, uses[[i]], uses[[j]])) {
                    dominatesAll=FALSE
                    break
                }
            }

            if (dominatesAll) {
                tmp=uses[[index]]
                uses[[index]]=uses[[i]]
                uses[[i]]=tmp
                attr(uses[[index]], "llvmDominates")=TRUE
                index=index+1   
                change=TRUE
                break       
            }
        }


        if (! change) break
        if (index==length(uses)) {
            attr(uses[[index]], "llvmDominates")=TRUE
            break
        }
    }
    return(uses)
}

insertItemInTree = function(obj, func, tree) {
    #loop through leaf

    for (i in 1:(length(tree))) {

        if (is.list(tree[[i]])) {
            if (i==length(tree)) break
            stop("hammertime")
        }

        if (dominates(func, obj, tree[[i]])) {
            #insert object in tree
            if (i>1) {
                newTree=tree[1:(i-1)]
                newTree[[i]]=obj
            } else {
                newTree=list(obj)
            }

            newTree=append(newTree, tree[i:length(tree)])
            return(newTree)
        } else if (! dominates(func, tree[[i]], obj)) {
            #create new subtree at this location
            subTree=list(list(obj), tree[i:length(tree)])

            if (i==1) return(list(subTree))

            tree[i:length(tree)]=NULL
            tree[[i]]=subTree
            return(tree)
        }
    }

    if (! is.list(tree[[length(tree)]])) {
        #append obj to tree
        tree[[length(tree)+1]]=obj
        return(tree)
    }


    #loop through subleafs

    subLeafs=tree[[length(tree)]]
    dominantSubLeafs=list()

    for (i in 1:(length(subLeafs))) {
        if (dominates(func, subLeafs[[i]][[1]], obj)) {
            subLeafs[[i]]=insertItemInTree(obj, func, subLeafs[[i]])
            tree[[length(tree)]]=subLeafs
            return(tree)
        } else if (dominates(func, obj, subLeafs[[i]][[1]])) {
            dominantSubLeafs=append(dominantSubLeafs, i)
        }
    }

    if (length(dominantSubLeafs) == 0) {

        subLeafs[[length(subLeafs)+1]]=list(obj)

    } else if (length(dominantSubLeafs) == 1) {
        subLeafs[[dominantSubLeafs[[1]]]]=insertItemInTree(obj, func, subLeafs[[dominantSubLeafs[[1]]]])
    } else {
        innerList=list()
        offset=0
        for (i in 1:length(dominantSubLeafs)) {
            innerList[[i]]=subLeafs[[dominantSubLeafs[[i]]-offset]]
            subLeafs[[dominantSubLeafs[[i]]-offset]]=NULL
            offset=offset+1
        }
        outerList=list(obj,innerList)
        subLeafs[[length(subLeafs)+1]]=outerList
    }
    tree[[length(tree)]]=subLeafs
    return(tree)
    
} 

getAllPredecessors = function(block, stopAt) {
    predsList=list()
    predsList[[getName(block)]]=block
    index=1

    while (length(predsList)>=index) {
        blockPred=getPredecessors(predsList[[index]])
        
        if (length(blockPred)>0) {
            for (j in 1:length(blockPred)) {
                if (getName(blockPred[[j]]) != getName(stopAt)) {
                    if (! getName(blockPred[[j]]) %in% names(predsList)) {
                        newEntry=list()
                        newEntry[[getName(blockPred[[j]])]]=blockPred[[j]]
                        predsList=append(predsList, newEntry)
                    }
                }
            }
        }
        index=index+1
    }
    return(predsList)
}

#this function prunes the tree
pruneTree = function(tree, op) {
    lastElement=tree[[length(tree)]]
    if (! is.list(lastElement)) return(tree)

    
    i=1

    if (length(lastElement)>1) {

        preds=list()
        removeLeaf=list()
        for (i in 1:length(lastElement)) {
            preds[[i]]=getAllPredecessors(getParent(lastElement[[i]][[1]]), getParent(op))
            removeLeaf[[i]]=FALSE
        }

        for (i in 1:(length(lastElement)-1)) {
            for (j in (i+1):length(lastElement)) {
                if (getName(getParent(lastElement[[i]][[1]])) %in% names(preds[[j]])) {
                    removeLeaf[[j]]=TRUE
                }
                if (getName(getParent(lastElement[[j]][[1]])) %in% names(preds[[i]])) {
                    removeLeaf[[i]]=TRUE
                }
            }
       }
        for (i in length(lastElement):1) {
            if (removeLeaf[[i]]) {
                lastElement[[i]]=NULL
            }
        }
    }

    if (length(lastElement) == 0) {
        tree[[length(tree)]]=NULL
        return(tree)
    }

    #browser()
    for (i in 1:length(lastElement)) {
        lastElement[[i]]=pruneTree(lastElement[[i]], op)
    }

    if (length(tree)==1 && length(lastElement)==1) {
        return(lastElement[[1]])
    }

    tree[[length(tree)]]=lastElement
    return(tree)
}


getAllValueUsersTree = 
function(obj) {
    #browser()
    uses=coerceGenericInstruction(.Call("R_Value_getAllUsers", as(obj, "Value")), throwError=FALSE)

    if (length(uses)<2) return(uses)

    #check if obj is a function

    if (is(obj, "Argument")) {
        func=getParent(obj)
    } else {
        bb=getParent(obj)

        if (! is(bb, "BasicBlock")) return(uses)

        func=getParent(bb)

        if (! is(func, "Function")) return(uses)
    }

    #remove duplicates

    i=1
    while (i<length(uses)) {
        for (j in (i+1):length(uses)) {
            if (llvmDump(uses[[i]]) == llvmDump(uses[[j]])) {
                uses[[j]]=NULL
                i=i-1
                break
            }
        }
        i=i+1
    }

    if (length(uses)<2) return(uses)

    #order uses by dominance

    

    tree=list(uses[[1]])


    for (i in 2:(length(uses))) {
        tree=insertItemInTree(uses[[i]], func, tree)
    }

    #browser()

    tree=pruneTree(tree, obj)

    return(tree)
}
