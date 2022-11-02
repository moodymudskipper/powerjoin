# paste_xy

    Code
      paste_xy(letters[1:3], c("d", NA, ""))
    Output
      [1] "a d" "b"   "c"  
    Code
      paste_yx(letters[1:3], c("d", NA, ""))
    Output
      [1] "d a" "b"   "c"  
    Code
      paste_xy(letters[1:3], c("d", NA, ""), na = NA, ignore_empty = FALSE)
    Output
      [1] "a d" NA    "c " 
    Code
      paste_xy(letters[1:3], c("d", NA, ""), na = "NA", ignore_empty = FALSE)
    Output
      [1] "a d"  "b NA" "c"   

