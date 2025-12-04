# Hw q6
Chaewoo Chun (A18548909)

All functions in R have at least 3 things:

-A **name**, we pick this and use it to call the function. - Input
**arguments**, there can be multiple comma seperated inputs to the
function - The **body**, lines of R code that do the work of the
function.

Our first wee function:

``` r
add <- function(x,y=1) {
  x+y
}
```

Let’s test out function

``` r
add(c(1,2,3), y=10)
```

    [1] 11 12 13

``` r
add(10,10)
```

    [1] 20

## A second function

Let’s try something more interesting. Make a sequence generation tool.

The ‘sample()’ function could be useful here.

``` r
sample(1:10, size = 3)
```

    [1]  7 10  8

change this to work with nucleotide A C G and T and return 3 of them

``` r
n <- c ("A","C","G","T")
sample(n,size= 20,replace= TRUE)
```

     [1] "C" "A" "C" "C" "C" "A" "G" "C" "C" "A" "T" "G" "G" "G" "T" "C" "C" "G" "C"
    [20] "C"

Turn this snippet into a function that returns a user specified length
DNA sequence… Let’s call it ’generate_dna()…

``` r
generate_dna <- function(len=10, fasta=FALSE) {
  n <- c ("A","C","G","T")
 v <-sample(n,size= len,replace= TRUE)
 
 # Make a single element vector 
 s<- paste(v, collapse="")
 
cat("Well done you!")
if(fasta){
return(s)
} else{return(v)
}
}
```

``` r
generate_dna(5)
```

    Well done you!

    [1] "T" "C" "T" "G" "G"

``` r
s <- generate_dna(15)
```

    Well done you!

``` r
s
```

     [1] "G" "G" "A" "C" "T" "A" "T" "C" "G" "C" "A" "A" "A" "T" "T"

I want the option to return a single element character vector with my
sequence all together like this: “GGAGTAC”

``` r
generate_dna(10,fasta= FALSE)
```

    Well done you!

     [1] "T" "C" "A" "G" "G" "C" "G" "T" "C" "G"

``` r
generate_dna(10,fasta= TRUE)
```

    Well done you!

    [1] "GTCAGATACC"

## A more advanced example

Make a third function that generates protein sequence of a user
specified length and format.

``` r
generate_protein <- function(size = 15, fasta = TRUE) {
  aa <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", 
          "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
  seq <- sample(aa, size = size, replace = TRUE)
 
   if (fasta) {
    return(paste(seq, collapse = ""))
  } else {
    return(seq)
  }
}
```

try this out,,,

``` r
generate_protein(10)
```

    [1] "KAIEDEMCYS"

``` r
generate_protein(10 , fasta= FALSE)
```

     [1] "E" "L" "Q" "N" "V" "N" "P" "V" "R" "W"

> Q. Generate random protein sequences between lengths 5 and 12 amino
> acids.

``` r
generate_protein(5)
```

    [1] "SNAQF"

``` r
generate_protein(12)
```

    [1] "YMIENPLTLVEY"

One approach is to do this by brute force calling our function for each
length 5 to 12.

Another approach is to write a ‘for()’ loop to itterate over the input
values 5 to 12

A very useful third R specific approach is to use the ‘sapply()’
function.

``` r
seq_lengths <- 5:12
for ( i in seq_lengths) {
  cat(">",i,"\n")
  cat(generate_protein(i))
      cat("\n")
}
```

    > 5 
    DWFQQ
    > 6 
    GINMNH
    > 7 
    NDHEQPF
    > 8 
    RFWHALNK
    > 9 
    CCMCQKYFA
    > 10 
    SNHPTDYTYD
    > 11 
    GWQQIAFCSFY
    > 12 
    GVMYPDSNSWYT

``` r
sapply(5:12, generate_protein)
```

    [1] "KIAPK"        "WVVYRF"       "MKLRALL"      "RDWAELQE"     "IWCNQAFCY"   
    [6] "IIQNKCGVDM"   "LELLRFYHINW"  "DQTKHWCLSTRA"

> **Key- Point**: Writing functions in R is doable but not the eaiset
> thing. Starting with a working snippet of code and then using LLM
> tools to improve generalize your function code is productive approach.

install.packages(“bio3d”) \# run once if needed

``` {r}
library(bio3d)
```

\##This function reads multiple PDB structures, extracts per-residue
B-factors for a selected chain and atom type, normalizes values,
overlays their profiles, and optionally performs clustering. The example
below compares 4AKE, 1AKE, and 1E4Y.

``` {r}

# helper

.norm01 <- function(x) {
r <- range(x, na.rm = TRUE)
if (!is.finite(r[1]) || diff(r) == 0) return(rep(NA_real_, length(x)))
(x - r[1]) / (r[2] - r[1])
}

# function

analyze_bfactors <- function(inputs,
chain = "A",
elety = "CA",
normalize = TRUE,
overlay = TRUE,
do_cluster = TRUE,
quiet = FALSE) {

# input checks

if (missing(inputs) || is.null(inputs)) stop("`inputs` is missing.")
inputs <- trimws(as.character(inputs)); inputs <- inputs[nzchar(inputs)]
if (length(inputs) < 1) stop("Provide at least one PDB ID or file path.")

# collect per-structure data

df_list <- list()
for (id in inputs) {
if (!quiet) message("Reading: ", id)
pdb <- read.pdb(id)                                # PDB ID or local path
trm <- trim.pdb(pdb, chain = chain, elety = elety) # keep chain/atom
if (is.null(trm) || is.null(trm$atom) || !nrow(trm$atom)) {
warning("Nothing after trim for: ", id, " (skipping)")
next
}
b <- trm$atom$b
if (normalize) b <- .norm01(b)
df_list[[length(df_list) + 1]] <- data.frame(
id = id, resno = trm$atom$resno, b = as.numeric(b)
)
}
if (!length(df_list)) stop("No valid structures after trimming.")
df <- do.call(rbind, df_list)

# align by residue number

res_all <- sort(unique(df$resno))
ids     <- unique(df$id)
mat <- matrix(NA_real_, nrow = length(res_all), ncol = length(ids),
dimnames = list(resno = res_all, id = ids))
for (id in ids) {
di <- df[df$id == id, c("resno", "b")]
ag <- aggregate(di$b, by = list(resno = di$resno), FUN = mean, na.rm = TRUE)
mat[as.character(ag$resno), id] <- ag$x
}

# overlay plot

if (overlay) {
op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
matplot(as.numeric(rownames(mat)), mat, type = "l", lty = 1, lwd = 2,
xlab = "Residue number",
ylab = if (normalize) "Normalized B-factor" else "B-factor",
main = "Per-residue B-factor profiles (aligned by residue number)")
legend("topright", legend = colnames(mat), lty = 1, lwd = 2, bty = "n")
}

# optional clustering

hc_obj <- NULL
if (do_cluster && ncol(mat) > 1) {
mat_imp <- apply(mat, 2, function(col) {
m <- mean(col, na.rm = TRUE)
if (is.nan(m)) return(col)
col[is.na(col)] <- m
col
})
d <- dist(t(mat_imp))
hc_obj <- hclust(d)
plot(hc_obj, main = "Similarity of B-factor profiles (hclust)")
}

invisible(list(
data_long   = df,
matrix_wide = mat,
hclust      = hc_obj
))
}

# example call: produces overlay plot + dendrogram and returns objects

res <- analyze_bfactors(
c("4AKE","1AKE","1E4Y"),
chain = "A",
elety = "CA",
normalize = TRUE,
overlay = TRUE,
do_cluster = TRUE,
quiet = FALSE
)

# small textual proof for the rubric

str(res$matrix_wide)
head(res$data_long)

# -----------------------------------------------------------------------------
```

\##The overlay plot shows flexibility (B-factor) across residues for
three adenylate kinase structures. The dendrogram clusters 4AKE and 1E4Y
together, indicating similar flexibility profiles.
