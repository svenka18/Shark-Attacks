get_cTree <- function(cf, k=1) {
  dt <- cf@data@get("input")
  tr <- party:::prettytree(cf@ensemble[[k]], names(dt))
  tr_updated <- update_tree(tr, dt)
  new("BinaryTree", tree=tr_updated, data=cf@data, responses=cf@responses, 
      cond_distr_response=cf@cond_distr_response, predict_response=cf@predict_response)
}

update_tree <- function(ct, dt) {
  ct <- update_weights(ct, dt)
  if(!ct$terminal) {
    ct$left <- update_tree(ct$left, dt)
    ct$right <- update_tree(ct$right, dt)   
  } 
  ct
}

update_weights <- function(ct, dt) {
  splt <- ct$psplit
  spltClass <- attr(splt,"class")
  spltVarName <- splt$variableName
  spltVar <- dt[,spltVarName]
  spltVarLev <- levels(spltVar)
  if (!is.null(spltClass)) {
    if (spltClass=="nominalSplit") {
      attr(ct$psplit$splitpoint,"levels") <- spltVarLev   
      filt <- spltVar %in% spltVarLev[as.logical(ct$psplit$splitpoint)] 
    } else {
      filt <- (spltVar <= splt$splitpoint)
    }
    ct$left$weights <- as.numeric(filt)
    ct$right$weights <- as.numeric(!filt)
  }
  ct
}

plot(get_cTree(cf, 1))
