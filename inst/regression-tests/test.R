
library(layoutEngine)

## function to generate backend for testing
testEngine <- function(type,
                       text="",
                       width=unit(1, "npc"), height=unit(1, "npc"),
                       font="sans", bold=FALSE, italic=FALSE,
                       size=12, color="black",
                       backgroundColor="transparent",
                       borderLeftWidth=0,
                       borderTopWidth=0,
                       borderRightWidth=0,
                       borderBottomWidth=0,
                       borderLeftStyle="solid",
                       borderTopStyle="solid",
                       borderRightStyle="solid",
                       borderBottomStyle="solid",
                       borderLeftColor="black",
                       borderTopColor="black",
                       borderRightColor="black",
                       borderBottomColor="black") {
    if (!is.unit(width)) width <- unit(width, "in")
    if (!is.unit(height)) height <- unit(height, "in")
    testLayout <- function(...) {
        layoutArgs <- list(type, paste0(type, ".1"), 0, 0,
                           convertWidth(width, "in", valueOnly=TRUE)*96,
                           convertHeight(height, "in", valueOnly=TRUE)*96,
                           0, text, font, bold, italic, size, color,
                           backgroundColor,
                           borderLeftWidth, borderTopWidth,
                           borderRightWidth, borderBottomWidth,
                           borderLeftStyle, borderTopStyle,
                           borderRightStyle, borderBottomStyle,
                           borderLeftColor, borderTopColor,
                           borderRightColor, borderBottomColor)
        do.call(makeLayout, layoutArgs)
    }
    makeEngine(testLayout)
}

pdf("tests.pdf")
grid.html("<p></p>",
          engine=testEngine("TEXT", "sans text", width=1, height=1))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine("TEXT", "sans bold text", width=1, height=1,
                            bold=TRUE))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine("TEXT", "sans italic text", width=1, height=1,
                            italic=TRUE))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine("TEXT", "red text", width=1, height=1,
                            color="red"))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine(c("CELL", "TEXT"),
                            "sans text\nyellow background",
                            width=1, height=1,
                            backgroundColor="yellow"))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine(c("CELL", "TEXT"),
                            "sans text\nwith border",
                            width=1, height=1,
                            borderLeftWidth=1, borderTopWidth=1,
                            borderRightWidth=1, borderBottomWidth=1))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine(c("CELL", "TEXT"),
                            "sans text\nthick border",
                            width=1, height=1,
                            borderLeftWidth=1, borderTopWidth=5,
                            borderRightWidth=1, borderBottomWidth=1))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine(c("CELL", "TEXT"),
                            "sans text\nred border",
                            width=1, height=1,
                            borderLeftWidth=1, borderTopWidth=1,
                            borderRightWidth=1, borderBottomWidth=1,
                            borderTopColor="red"))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine(c("CELL", "TEXT"),
                            "sans text\ndotted border",
                            width=1, height=1,
                            borderLeftWidth=1, borderTopWidth=1,
                            borderRightWidth=1, borderBottomWidth=1,
                            borderTopStyle="dotted"))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine(c("CELL", "TEXT"),
                            "sans text\ndashed border",
                            width=1, height=1,
                            borderLeftWidth=1, borderTopWidth=1,
                            borderRightWidth=1, borderBottomWidth=1,
                            borderTopStyle="dashed"))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine(c("CELL", "TEXT"),
                            "sans text\nhidden border",
                            width=1, height=1,
                            borderLeftWidth=1, borderTopWidth=1,
                            borderRightWidth=1, borderBottomWidth=1,
                            borderTopStyle="hidden"))
grid.newpage()
grid.html("<p></p>",
          engine=testEngine(c("CELL", "TEXT"),
                            "cell with\nviewport",
                            width=1, height=1),
          viewports=TRUE)
downViewport("CELL.1.vp")
grid.rect()
dev.off()

## Check graphical output
testoutput <- function(basename) {
    PDF <- paste0(basename, ".pdf")
    savedPDF <- system.file("regression-tests", paste0(basename, ".save.pdf"),
                            package="layoutEngine")
    system(paste0("pdfseparate ", PDF, " test-pages-%d.pdf"))
    system(paste0("pdfseparate ", savedPDF, " model-pages-%d.pdf"))
    modelFiles <- list.files(pattern="model-pages-.*[.]pdf")
    N <- length(modelFiles)
    allGood <- TRUE
    for (i in 1:N) {
        system(paste0("convert -density 96 ",
                      "model-pages-", i, ".pdf ",
                      "model-pages-", i, ".png"))
        system(paste0("convert -density 96 ",
                      "test-pages-", i, ".pdf ",
                      "test-pages-", i, ".png"))
        result <- system(paste0("compare -metric AE ",
                                "model-pages-", i, ".png ",
                                "test-pages-", i, ".png ",
                                "diff-pages-", i, ".png ",
                                "2>&1"), intern=TRUE)
        if (result != "0") {
            cat(paste0("Test and model differ (page ", i, "; ",
                       "see diff-pages-", i, ".png)\n"))
            allGood <- FALSE
        }
    }
    if (!allGood)
        stop("Regression testing detected differences")
}

testoutput("tests")
