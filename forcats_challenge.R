install.packages("markdown")
library(markdown)
install.packages("tidyverse")
library(tidyverse)
library(forcats)

f <- factor(sample(letters[1:3], 20, replace = TRUE))
f
fct_expand(f, "d", "e", "f")
fct_expand(f, letters[1:6])

x <- factor(rep(LETTERS[1:9], times = c(40, 10, 5, 27, 1, 1, 1, 1, 1)))

fct_other(x, keep = c("A", "B"))
fct_other(x, drop = c("A", "B"))

chks <- subset(ChickWeight, as.integer(Chick) < 10)
chks <- transform(chks, Chick = fct_shuffle(Chick))
if (require("ggplot2")) {
    ggplot(chks, aes(Time, weight, colour = Chick)) +
        geom_point() +
        geom_line()
    
    # Note that lines match order in legend
    ggplot(chks, aes(Time, weight, colour = fct_reorder2(Chick, Time, weight))) +
        geom_point() +
        geom_line() +
        labs(colour = "Chick")
}

f <- factor(c("a", "b", "c"))
fct_rev(f)

x <- factor(
    c("Mon", "Tue", "Wed"),
    levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
    ordered = TRUE
)
x
fct_shift(x)

fct_shift(x, 2)

fct_shift(x, -1)

f <- factor(c("a", "b", "c"))
fct_shuffle(f)
fct_shuffle(f)

fs <- list(factor("a"), factor("b"), factor(c("a", "b")))
fct_unify(fs)

f <- factor(letters[rpois(100, 10)])

unique(f)     # in order of appearance
fct_unique(f) # in order of levels

gss_cat

fct_count(gss_cat$relig)
fct_count(fct_lump(gss_cat$relig))


f <- factor(c("a", "b", "c"))
lvls_reorder(f, 3:1)
lvls_revalue(f, c("apple", "banana", "carrot"))
lvls_expand(f, c("a", "b", "c", "d"))
f

fs <- list(factor("a"), factor("b"), factor(c("a", "b")))
lvls_union(fs)

x <- c("a", "z", "g")
as_factor(x)
as.factor(x)

gss_cat$relig %>% fct_count()
gss_cat$relig %>% fct_anon() %>% fct_count()
gss_cat$relig %>% fct_anon() %>% fct_count()
gss_cat$relig %>% fct_anon("X") %>% fct_count()

fa <- factor("a")
fb <- factor("b")
fab <- factor(c("a", "b"))

c(fa, fb, fab)
fct_c(fa, fb, fab)

# You can also pass a list of factors as the first argument
fs <- list(fa, fb, fab)
fct_c(fs)

fct_count(gss_cat$partyid)

partyid2 <- fct_collapse(gss_cat$partyid,
                         missing = c("No answer", "Don't know"),
                         other = "Other party",
                         rep = c("Strong republican", "Not str republican"),
                         ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                         dem = c("Not str democrat", "Strong democrat")
)
fct_count(partyid2)

f <- factor(sample(letters)[rpois(1000, 10)])
table(f)
fct_count(f)
fct_count(f, sort = TRUE)

f <- factor(c("a", "b"), levels = c("a", "b", "c"))
f
fct_drop(f)

# Set only to restrict which levels to drop
fct_drop(f, only = "a")
fct_drop(f, only = "c")

f <- factor(c("b", "b", "a", "c", "c", "c"))
f
fct_inorder(f)
fct_infreq(f)

fct_inorder(f, ordered = TRUE)

