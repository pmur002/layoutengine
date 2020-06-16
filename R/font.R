
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

## Font weight to numeric
## Ref. https://www.w3.org/TR/css-fonts-3/#font-weight-prop
fontWeightMap <-
    data.frame(
        fcWeights = c("thin", "ultralight", "light", "normal", "medium",
                      "semibold", "bold", "ultrabold", "heavy"),
        cssWeights = seq(100, 900, 100),
        stringsAsFactors=FALSE)

mapFCfontWeight <- function(weight) {
    fontWeightMap$cssWeights[match(weight, fontWeightMap$fcWeights)]
}

cssFontFace <- function(name, style, weight, file) {
    paste("@font-face {",
          paste0('  font-family: "', name, '";'),
          paste0("  font-style: ", style, ";"),
          paste0("  font-weight: ", weight, ";"),
          paste0("  src: url('assets/", basename(file), "');"),
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
                             fontFile <- as.character(sf$path[which])
                             if (!is.null(cssTransform$fontFile))
                                 fontFile <-
                                     cssTransform$fontFile(fontFile)
                             cssFontFace(as.character(sf$family[which]),
                                         fontStyle(sf$italic[which]),
                                         fontWeight,
                                         fontFile)
                         }))
    paste(css, collapse="\n")
}

checkMissingType1Fonts <- function(fonts, fontTable) {
    missing <- !(fonts %in% fontTable$family)
    if (any(missing)) {
        for (i in which(missing)) {
            ## Try font names with white space removed
            trimmedFont <- gsub(" ", "", fonts[i])
            if (trimmedFont %in% fontTable$family) {
                fonts[i] <- trimmedFont
                missing[i] <- FALSE
            } else {
                stop(paste0("Font(s) not available: ", fonts[missing]))
            }
        }
    }
    fonts
}


## Look up font in extrafont::fonttable and
## register each font face
Type1FontCSS <- function(fonts, cssTransform) {
    ft <- get("fontTable", envir=layoutEngineEnv)
    mappedFonts <- mapGenericFonts(fonts)
    checkedFonts <- checkMissingType1Fonts(mappedFonts, ft)
    css <- unlist(lapply(checkedFonts,
                         function(font) {
                             which <- ft$family == font
                             fontFile <- as.character(ft$path[which])
                             if (!is.null(cssTransform$path))
                                 fontFile <-
                                     cssTransform$path(fontFile)
                             cssFontFace(as.character(ft$family[which]),
                                         fontStyle(ft$italic[which]),
                                         fontWeight(ft$bold[which]),
                                         fontFile)
                         }))
    paste(css, collapse="\n")
}

pdfFontCSS <- function(fonts, cssTransform) {
    Type1FontCSS(fonts, cssTransform)
}    

postscriptFontCSS <- pdfFontCSS

## Generate CSS @font-face rules based on 'fonts'
fontCSS <- function(fonts, device, cssTransform) {
    if (device == "postscript") {
        postscriptFontCSS(fonts, cssTransform)
    } else if (device == "pdf") {
        pdfFontCSS(fonts, cssTransform)
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
                      which <- ft$family == font
                      as.character(ft$path[which])
                  }))
}

cairoFontFiles <- function(fonts) {
    sf <- get("sysFonts", envir=layoutEngineEnv)
    fonts <- mapGenericFonts(fonts)
    checkMissingCairoFonts(fonts)
    unlist(lapply(fonts,
                  function(fontname) {
                      which <- sf$family == fontname
                      as.character(sf$path[which])
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

Type1FontFamily <- function(fonts) {
    ft <- get("fontTable", envir=layoutEngineEnv)
    mappedFonts <- mapGenericFonts(fonts)
    checkMissingType1Fonts(mappedFonts, ft)
}

cairoFontFamily <- function(fonts) {
    sf <- get("sysFonts", envir=layoutEngineEnv)
    fonts <- mapGenericFonts(fonts)
    checkMissingCairoFonts(fonts)
    fonts
}

cssFontFamily <- function(fonts, device=currentDevice()) {
    if (device %in% c("postscript", "pdf")) {
        cssFonts <- Type1FontFamily(fonts)
    } else if (cairoDevice(device)) {
        cssFonts <- cairoFontFamily(fonts)
    } else {
        stop(paste0("Device ", device, " unsupported"))
    }
    names(cssFonts) <- fonts
    cssFonts
}
