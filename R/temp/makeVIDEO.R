#' @title makeVIDEO
#' @description make video through ffmpeg
#' 
#' @param file filename of output video
#' @param path The path of figures, i.e, pngs, to make video
#' @param pattern regular expression pattern to filter figures
#' @param mode must be one of "ultrafast", "slow", "veryslow"
#' 
#' @export
makeVIDEO <- function(file = "ffmpeg_%d.avi", 
                      path = ".", pattern = "*.png",
                      mode = c("ultrafast", "slow", "veryslow")){
  mode <- mode[1]
  system(sprintf("(for %%i in (%s\\%s) do @echo file '%%i') > list.txt", 
                gsub("/", "\\\\", path), pattern))
  system(sprintf("ffmpeg -safe 0 -f concat -r 2 -i list.txt -c:v libx264 -preset %s -crf 0 %s", 
                mode, gsub("/", "\\\\", file) ))
  file.remove("list.txt") #remove list.txt, return null
}
