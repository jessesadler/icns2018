## Demonstration of arithmetic with debkeepr ##

library(debkeepr)

# Normalization: £10 52s. 76d.
deb_normalize(lsd = c(10, 52, 76))

# Multiplication: £15 3s. 8d. by 32
deb_multiply(lsd = c(15, 3, 8), x = 32)

# Division: £465 12s. 8d. by 72
deb_divide(lsd = c(465, 12, 8), x = 72)

# 15783 pence to pounds, shillings, and pence
deb_d_lsd(d = 15783)

# Running account of Jacques and Daniel
# 15 February 1585 to 23 May 1585
# sum with deb_sum_df

l <- c(26, 8, 5, 28, 4, 369, 85, 15, 2, 0, 155, 50, 120, 9, 2, 213, 71, 17, 93, 3)
s <- c(4, 0, 1, 10, 7, 4, 4, 18, 18, 3, 1, 8, 11, 10, 9, 6, 5, 5, 3, 16)
d <- c(0, 0, 10, 0, 8, 4, 11, 10, 6, 4, 4, 0, 0, 8, 3, 8, 0, 10, 1, 11)

dvdm_57_85 <- data.frame(l = l,
                         s = s,
                         d = d)

deb_sum_df(dvdm_57_85, l = l, s = s, d = d)
