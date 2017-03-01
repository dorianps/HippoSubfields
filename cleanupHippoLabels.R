# function to cleanup spare voxels
# it goes slice by slice, and, for each label
# removes clusters with less than `thresh` voxels
# inputs:
# img - antsImage
# thresh - a positive number
# output:
# newimg - antsImage cleaned
cleanupHippoLabels = function (img, thresh, skiplabels = c(2,3,4)) {  # default skips CA2, DG, CA3
  # find label numbers quickly
  labels = as.matrix(1:max(img))
  sums = apply(labels, 1, function(x) sum(img==x) )
  labels = labels[sums!=0]
  
  labelslicelimits = data.frame(min=rep(0,length(labels)),
                                max=rep(0,length(labels)) )
  rownames(labelslicelimits) = labels
  for (label in labels) {
    labelslicelimits[as.character(label), ] = labelLimits(img, label=label)
  }
  
  # find slice span
  slicelimits = labelLimits(img)
  slicearr = slicelimits[1]:slicelimits[2]
  newimg = img*0
  
  for (slice in slicearr) {
    slimg = cropIndices(img, c(1,1,slice), c(dim(img)[1],dim(img)[2],slice))
    for (label in labels) {
      # skip ignored labels
      if (label %in% skiplabels) next
      # skip if empty label
      if (sum(slimg==label) == 0) next
      # skip if within 2 slices from start or end
      norunlabelslices = (labelslicelimits[as.character(label), 1] + 2):
                         (labelslicelimits[as.character(label), 2] - 2)
      if (slice %in% norunlabelslices) next
      
      labimg = slimg*0
      labimg[slimg==label] = 1
      overthreshimg = labelClusters(labimg, minClusterSize = thresh)
      slimg[slimg==label]=0
      slimg[overthreshimg>0]=label    
    }
    newimg[ 1:dim(slimg)[1], 1:dim(slimg)[2], slicearr[slicearr==slice] ] = slimg[slimg>-1]
  }

  return(newimg)
}


labelLimits = function(img, label=NULL) {
  if (is.null(label)) { # consider all > 0
    img=antsImageClone(img)
    img[img>0]=1
    label=1
  }
  if (sum(img==label)==0) {
    return(NA)
  }
  limit=img*0; limit[img==label]=1
  temp = which( apply(as.array(limit), 3, sum) !=0)
  return(c(min(temp), max(temp)))
}
