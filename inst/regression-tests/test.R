
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
                           text, font, bold, italic, size, color,
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
dev.off()

## Check graphical output
savedPDF <- system.file("regression-tests", "tests.save.pdf",
                        package="layoutEngine")
diff <- tools::Rdiff("tests.pdf", savedPDF)

if (diff != 0L) {
    ## If differences found, generate images of the differences and error out
    system("pdfseparate tests.pdf test-pages-%d.pdf")
    system(paste0("pdfseparate ", savedPDF, " model-pages-%d.pdf"))
    modelFiles <- list.files(pattern="model-pages-.*")
    N <- length(modelFiles)
    for (i in 1:N) {
        system(paste0("compare model-pages-", i, ".pdf ",
                      "test-pages-", i, ".pdf ",
                      "diff-pages-", i, ".png"))
    } 
    stop("Regression testing detected differences")
}
