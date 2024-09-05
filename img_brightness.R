library(imager)

# load image
dog <- load.image("https://3.bp.blogspot.com/-Vxpx9a4a75U/UuJd81EYS-I/AAAAAAAACXk/NZWg4oJYpgc/s1600/Dog-Picture6.jpg")

# function to change brightness of image 
# positive value of bright increases luminicance and negative value decreases it
change_brightness <- function(img,bright = 0.2)
{
    # access the 3D array of image
  img.mat <- as.array(img[ , ,1, ])
  dims <- dim(img.mat)  # store dimensions
  changed_mat <- array(0,dims) # matrix of zeroes to store new image matrix
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      new_vec <- numeric(length = 3)
      new_vec <- img.mat[i,j, ] + c(bright,bright,bright)

      for(k in 1:3)
      {
        if(new_vec[k]<0)
        {
          new_vec[k] <- 0
        }
        if(new_vec[k]>1)
        {
          new_vec[k] <- 1
        }
      }
      changed_mat[i,j, ] <- new_vec

    }
  }
  
  changed_img <- as.cimg(changed_mat)
  return(changed_img)
}
par(mfrow = c(1,2)); # for 2 plots
plot(dog)
plot(change_brightness(dog,bright = -0.3))