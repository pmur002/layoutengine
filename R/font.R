
## Font definitions to use when laying out HTML

## Convert logical italic to character style
fontStyle <- function(italic) {
    ifelse(italic, "italic", "normal")
}

## Convert logical bold to character weight
fontWeight <- function(bold) {
    ifelse(bold, "bold", "normal")
}

## fontconfig font weights do not correspond directly to
## CSS font weights
## https://lists.freedesktop.org/archives/fontconfig/2011-September/003646.html
## https://lists.freedesktop.org/archives/fontconfig/2011-September/003647.html
## https://github.com/servo/libfontconfig/blob/master/fontconfig/fontconfig.h

## See HTML/fontWeights/
fontWeightMap <-
    structure(list(fcWeights = 0:215, cssWeights = c(100, 100, 100, 
101, 101, 101, 102, 102, 103, 104, 105, 105, 106, 108, 109, 110, 
112, 113, 115, 117, 118, 120, 123, 125, 127, 130, 133, 136, 139, 
142, 146, 149, 153, 158, 162, 167, 173, 178, 185, 192, 200, 209, 
219, 230, 243, 255, 266, 277, 286, 294, 301, 307, 312, 317, 321, 
325, 329, 333, 336, 339, 342, 345, 348, 351, 354, 356, 359, 362, 
364, 367, 369, 372, 375, 378, 380, 383, 386, 390, 393, 396, 400, 
404, 409, 413, 418, 423, 428, 434, 439, 445, 451, 457, 462, 468, 
473, 478, 484, 488, 493, 497, 501, 505, 508, 512, 515, 518, 520, 
523, 525, 528, 530, 532, 534, 536, 537, 539, 541, 542, 543, 544, 
546, 547, 548, 549, 550, 551, 551, 552, 553, 554, 554, 555, 555, 
556, 556, 557, 557, 558, 558, 559, 559, 560, 560, 560, 561, 561, 
562, 562, 563, 563, 563, 564, 564, 565, 566, 566, 567, 567, 568, 
569, 569, 570, 571, 572, 573, 574, 575, 576, 577, 579, 580, 581, 
583, 585, 586, 588, 590, 592, 594, 596, 599, 601, 604, 607, 610, 
613, 617, 620, 624, 628, 633, 637, 642, 647, 653, 659, 666, 673, 
681, 690, 700, 712, 727, 747, 770, 797, 821, 843, 863, 883, 903, 
922, 942, 961, 980, 1000)), .Names = c("fcWeights", "cssWeights"
), row.names = c(NA, -216L), class = "data.frame")

mapFCfontWeight <- function(weight) {
    fontWeightMap$cssWeights[match(weight, fontWeightMap$fcWeights)]
}

cssFontFace <- function(name, style, weight, file) {
     paste("@font-face {",
           paste0('  font-family: "', name, '";'),
           paste0("  font-style: ", style, ";"),
           paste0("  font-weight: ", weight, ";"),
           paste0("  src: url('", basename(file), "');"),
           "}",
           sep="\n")
}

## Turn generic font families into specific fonts
mapGenericFonts <- function(fonts) {
    generic <- grepl("^(sans|serif|mono)$", fonts)
    if (any(generic)) {
        ## R's Cairo devices perform the following subsitutions (see ?X11)
        genericMap <- c(sans="Helvetica", serif="Times", mono="Courier")
        fonts[generic] <- genericMap[fonts[generic]]
        ## Find best match for substituted font
        ## (to emulate what R graphics device will do)
        fonts[generic] <- sapply(fonts[generic], match_family)
    }
    fonts
}

checkMissingCairoFonts <- function(fonts) {
    missing <- !sapply(fonts, font_family_exists)
    if (any(missing))
        stop(paste0("Font(s) not available: ", fonts[missing]))
}

## Look up system fonts and register all variations 
cairoFontCSS <- function(fonts, cssTransform) {
    sf <- get("sysFonts", envir=layoutEngineEnv)
    fonts <- mapGenericFonts(fonts)
    checkMissingCairoFonts(fonts)
    css <- unlist(lapply(fonts,
                         function(font) {
                             which <- sf$family == font
                             fontWeight <- mapFCfontWeight(sf$weight[which])
                             if (!is.null(cssTransform$fontWeight)) 
                                 fontWeight <-
                                     cssTransform$fontWeight(fontWeight)
                             cssFontFace(as.character(sf$family[which]),
                                         fontStyle(sf$slant[which] > 0),
                                         fontWeight,
                                         as.character(sf$file[which]))
                         }))
    paste(css, collapse="\n")
}

checkMissingType1Fonts <- function(fonts, fontTable) {
    missing <- !(fonts %in% fontTable$FamilyName)
    if (any(missing)) {
        for (i in which(missing)) {
            ## Try font names with white space removed
            trimmedFont <- gsub(" ", "", fonts[i])
            if (trimmedFont %in% fontTable$FamilyName)
                fonts[i] <- trimmedFont
            else
                stop(paste0("Font(s) not available: ", fonts[missing]))
        }
    }
    fonts
}


## Look up font in extrafont::fonttable and
## register each font face
Type1FontCSS <- function(fonts) {
    ft <- get("fontTable", envir=layoutEngineEnv)
    mappedFonts <- mapGenericFonts(fonts)
    checkedFonts <- checkMissingType1Fonts(mappedFonts, ft)
    css <- unlist(lapply(checkedFonts,
                         function(font) {
                             which <- ft$FamilyName == font
                             cssFontFace(as.character(ft$FamilyName[which]),
                                         fontStyle(ft$Italic[which]),
                                         fontWeight(ft$Bold[which]),
                                         as.character(ft$fontfile[which]))
                         }))
    paste(css, collapse="\n")
}

postscriptFontCSS <- function(fonts) {
    Type1FontCSS(fonts)
}

pdfFontCSS <- function(fonts) {
    Type1FontCSS(fonts)
}    

## Generate CSS @font-face rules based on 'fonts'
fontCSS <- function(fonts, device, cssTransform) {
    if (device == "postscript") {
        postscriptFontCSS(fonts)
    } else if (device == "pdf") {
        pdfFontCSS(fonts)
    } else if (cairoDevice(device)) {
        cairoFontCSS(fonts, cssTransform)
    } else {
        stop(paste0("Device ", device, " unsupported"))
    }
}

Type1FirstFont <- function(fonts) {
    ft <- get("fontTable", envir=layoutEngineEnv)
    mappedFonts <- mapGenericFonts(fonts)
    checkedFonts <- checkMissingType1Fonts(mappedFonts, ft)
    checkedFonts[1]
}

cairoFirstFont <- function(fonts) {
    sf <- get("sysFonts", envir=layoutEngineEnv)
    fonts <- mapGenericFonts(fonts)
    checkMissingCairoFonts(fonts[1])
    fonts[1]
}

## Get first font family (may be translated, e.g., if generic font)
firstFont <- function(fonts, device) {
    if (device %in% c("postscript", "pdf")) {
        Type1FirstFont(fonts)
    } else if (cairoDevice(device)) {
        cairoFirstFont(fonts)
    } else {
        stop(paste0("Device ", device, " unsupported"))
    }
}

Type1FontFiles <- function(fonts) {
    ft <- get("fontTable", envir=layoutEngineEnv)
    mappedFonts <- mapGenericFonts(fonts)
    checkedFonts <- checkMissingType1Fonts(mappedFonts, ft)
    unlist(lapply(checkedFonts,
                  function(font) {
                      which <- ft$FamilyName == font
                      as.character(ft$fontfile[which])
                  }))
}

cairoFontFiles <- function(fonts) {
    sf <- get("sysFonts", envir=layoutEngineEnv)
    fonts <- mapGenericFonts(fonts)
    checkMissingCairoFonts(fonts)
    unlist(lapply(fonts,
                  function(fontname) {
                      which <- sf$family == fontname
                      as.character(sf$file[which])
                  }))
}

## Generate font file paths based on 'fonts'
fontFiles <- function(fonts, device) {
    if (device %in% c("postscript", "pdf")) {
        Type1FontFiles(fonts)
    } else if (cairoDevice(device)) {
        cairoFontFiles(fonts)
    } else {
        stop(paste0("Device ", device, " unsupported"))
    }
}
