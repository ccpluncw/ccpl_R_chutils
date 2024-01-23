#' A function to resize images.
#'
#' This function resizes images.
#' @param input_file a string specifying the name of the input file containing the images.
#' @param output_file a string specifying the name of the output file that will contain the resized images.
#' @param width an integer specifying the new width of the image in pixels.
#' @param height an integer specifying the new height of the image in pixels.
#' @param keepAspectRatio A boolean specifying whether the orginal aspect ratio should be kept. If TRUE, then specify either the width or the height of the new image - NOT BOTH. DEFAULT = TRUE.
#' @keywords jpg resize image
#' @return Nothing.
#' @export
#' @examples ch.resize_jpg ("inputImage.jpg", "outputImageFile.jpg", width = 200, keepAspectRatio=T)

ch.resize_jpg <- function(input_file, output_file, width = NULL, height = NULL, keepAspectRatio = T) {
  # Check if the input file exists
  if (!file.exists(input_file)) {
    stop("The input file does not exist")
  }

  # Read the input image
  img <- magick::image_read(input_file)

  if(is.null(width) & is.null (height)){
    stop("must specify width or height")
  }

  aspectRatio <- magick::image_info(img)$width/magick::image_info(img)$height
  if(keepAspectRatio) {
    if(is.null(width)){
      width = magick::image_info(img)$width*(height/magick::image_info(img)$height)
    } else {
      height = magick::image_info(img)$height*(width/magick::image_info(img)$width)
    }

  }

  # Resize the image
  imgSize <- paste(width,height, sep="x")
  resized_img <- magick::image_resize(img, imgSize)

  # Write the resized image to the output file
  image_write(resized_img, output_file, format = "jpg", quality=100)
}
