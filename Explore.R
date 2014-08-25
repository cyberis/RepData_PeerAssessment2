#Explore.R - Contains exploratory data ananlysis
# How many different property damage exponents do we have?
unique(data$propdmgexp)
# [1] "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h"
#[15] "7" "H" "-" "1" "8"
# We have 19 of them some seem to be a power of ten and 
# most of the rest of them can be made into powers of ten
# Here is how it is distributed
table(data$propdmgexp)
#             -      ?      +      0      1      2      3      4 
# 465934      1      8      5    216     25     13      4      4 
#      5      6      7      8      B      h      H      K      m 
#     28      4      5      1     40      1      6 424665      7 
#      M 
#  11330 

# How Many different crop damage exponents do we have?
unique(data$cropdmgexp)
# [1] ""  "M" "K" "m" "B" "?" "0" "k" "2"
# It looks like we have 9 of a similar character to the
# property damage exponents
# Here is how that is distributed
table(data$cropdmgexp)
#             ?      0      2      B      k      K      m      M 
# 618413      7     19      1      9     21 281832      1   1994 

#How many types of event do we have
length(unique(data$evtype))
#[1] 985
# Wow that's alot. Let see what happens if we fix the case
length(unique(data$eventType))
