
app_server <- function(input, output, session)
{
library(shiny)
# Define server logic required to draw a histogram
server <- function(input, output) {
    AbstractsStrings <- NULL
    observeEvent(input$DoSubmitText,
        {
            AbstractsStrings <<- read.csv(input$file1$datapath)
            AbstractsStrings <<- as.character(AbstractsStrings[,1])
            output$result2 <- renderText(
                                  {
                                    paste("Text Successfully Uploaded")
                                  }
                                )
        }
                )
    IDs <- NULL
    dpath <- NULL
    observeEvent(input$DoSubmitUniqueIDs,
        {
            IDs <<- read.csv(paste(input$file2$datapath))
            dpath <<- paste(input$file2$datapath)
            IDs <<- as.character(IDs[,1])
            output$result3 <- renderText(
                                  {
                                    paste("UniqueIDs Successfully Uploaded")
                                  }
                                )
        }
                )
    ChosenKingdom <- NULL
    AbstractsSpp <- NULL
    SpeciesAbbrvs <- NULL
    GenesOut <- NULL
    GeneGroups <- NULL
    GeneGrades <- NULL
    AbsPhen <- NULL
     observeEvent(input$DoMining,
        {
            ChosenKingdom <<- input$King
            show_modal_spinner(text = "Mining species, this may take a few minutes...")
            AbstractsSpp <<- G2PMineR::SpeciesLookeR(AbstractsStrings,IDs, Kingdom = ChosenKingdom, Add = NULL)
            SpeciesAbbrvs <<- G2PMineR::SpeciesAbbreviatoR(AbstractsSpp)
            remove_modal_spinner()
            show_modal_spinner(text = "Mining genes, this may take a few minutes...")
            GenesOut <<- G2PMineR::GenesLookeR(AbstractsStrings, IDs, Kingdom = ChosenKingdom, Add = NULL, SppAbbr = SpeciesAbbrvs)
            GenesOut <<- G2PMineR::SynonymReplaceR(GenesOut, Kingdom = ChosenKingdom)
            GeneGroups <<- G2PMineR::GeneNamesGroupeR(as.vector(GenesOut[,1]))
            GeneGrades <<- G2PMineR::UtilityGradeR(GenesOut, Kingdom = ChosenKingdom, Add = NULL, Groups = as.data.frame(GeneGroups))
            remove_modal_spinner()
            show_modal_spinner(text = "Mining phenotypes, this may take a few minutes...")
            AbsPhen <<- G2PMineR::PhenotypeLookeR(AbstractsStrings, IDs, Kingdom = ChosenKingdom, Add = NULL)
            # Calculate proportion of abstracts with at least one G match
            GenProp <- G2PMineR::AbstractsProportionCalculator(GenesOut, IDs)
            # Calculate proportion of abstracts with at least one Ta match
            SppProp <- G2PMineR::AbstractsProportionCalculator(AbstractsSpp, IDs)
            # Calculate proportion of abstracts with at least one P match
            PhenProp <- G2PMineR::AbstractsProportionCalculator(AbsPhen, IDs)




            remove_modal_spinner()
            output$result4 <- renderText(
                                  {
                                    paste("Mining Complete")
                                  }
                                )
            output$result5 <- renderText(
                                  {
                                      paste(sprintf("%s percent of abstracts have at least one phenotype match",PhenProp*100))
                                      })
            output$result6 <- renderText(
                                  {
                                      paste(sprintf("%s percent of abstracts have at least one gene match",GenProp*100))
                                      })
            output$result7 <- renderText(
                                  {
                                      paste(sprintf("%s percent of abstracts have at least one species match",SppProp*100))
                                      })
        }
     )
        GenesOutLong <- NULL
        AbstractsSppLong <- NULL
        AbsPhenLong <- NULL
        ConsensusVenn <- NULL
        All_Consensus <- NULL
        SppBarPlotDF <- NULL
        GenesBarPlotDF <- NULL
        PhenoBarPlotDF <- NULL
         observeEvent(input$DoCons,
        {
            show_modal_spinner(text = "Performing consensus calculations, this may take a few minutes...")
            # Make gene mining results longform
            GenesOutLong <<- G2PMineR::MakeGenesOutLongform(GenesOut)
            # Make taxonomy mining results longform
            AbstractsSppLong <<- G2PMineR::MakeAbstractsSppLongform(AbstractsSpp)
            # Make phenotype words mining results longform
            AbsPhenLong <<- G2PMineR::MakeAbsPhenLongform(AbsPhen)
            # To make a consensus of all three
            All_Consensus <<- G2PMineR::ConsensusInferreR(Ta = AbstractsSppLong,
                                            G = GenesOutLong,
                                            P = AbsPhenLong,
                                            AbstractsSpp = AbstractsSpp,
                                            GenesOut = GenesOut,
                                            AbsPhen = AbsPhen)
            ConsensusVenn <<- All_Consensus$Venn
            # Make T matches barplot matrix
            SppBarPlotDF <<- G2PMineR::MatchesBarPlotteR(AbstractsSpp$Species, AbstractsSpp$Matches,n = 25)
            # Clean G output
            Genez <- as.data.frame(GenesOut[which(GenesOut$InOrNot == "Yes"),]) #select only those with at least one match
            Genez <- data.frame(Genez$Gene,Genez$Matches) #form new matrix
            Genez <- unique(Genez) #make it unique
            # Make G matches barplot matrix
            GenesBarPlotDF <<- G2PMineR::MatchesBarPlotteR(Genez[,1], Genez[,2], n = 25)
            # Make P matches barplot matrix
            PhenoBarPlotDF <<- G2PMineR::MatchesBarPlotteR(AbsPhen$PhenoWord, AbsPhen$AbsMatches,n = 25)
            # Make Ta barplot
            output$PlotBarSpp <<- renderPlot(barplot(SppBarPlotDF[,2], names.arg = SppBarPlotDF[,1], las=2, ylim = c(0,250), ylab = "Number of Absracts"))
            # Make G barplot
            output$PlotBarGen <- renderPlot(barplot(GenesBarPlotDF[,2], names.arg = GenesBarPlotDF[,1], las=2, ylim = c(0,100), ylab = "Number of Absracts"))
            # Make P barplot
            output$PlotBarPhen <- renderPlot(barplot(PhenoBarPlotDF[,2], names.arg = PhenoBarPlotDF[,1], las=2, ylim = c(0,1000), ylab = "Number of Absracts",cex.names =0.75))
            remove_modal_spinner()
            library(VennDiagram)
            output$PlotCV <- renderPlot(grid.draw(ConsensusVenn))
            output$result8 <- renderText(
                                  {
                                    paste("Consensus Calculations Complete")
                                  }
                                )
        }
                )
         TaS <- NULL
         TaG <- NULL
         TaP <- NULL
         Genez <- NULL
         rwmsS <- NULL
         rwmsG <- NULL
         rwmsP <- NULL
         SppIntSmall <- NULL
         GenIntSmall <- NULL
         PhenIntSmall <- NULL
         observeEvent(input$DoINA,
        {
            show_modal_spinner(text = "Performing internal network analysis, this may take a few minutes...")
            # Make gene mining results longform

print("Calculate internal matchwise distance matrix for Ta")
SppInt <- G2PMineR::InternalPairwiseDistanceInferreR(AbstractsSpp$Species, AbstractsSpp$Matches, allabsnum = length(IDs))
# Select only the top 50 (Optional, recommended for clarity)
SppIntSmall <<- G2PMineR::TopN_PickeR_Internal(SppInt, n = 50, decreasing = T)
# Assign result to distance matrix class
rwmsS <<- as.dist(SppIntSmall, diag = F, upper = FALSE)

# Clean G output
Genez <- as.data.frame(GenesOut[which(GenesOut$InOrNot == "Yes"),]) #select only those with at least one match
Genez <- data.frame(Genez$Gene,Genez$Matches) #form new matrix
Genez <<- unique(Genez) #make it unique

print("Calculate internal matchwise distance matrix for G")
GenInt <- G2PMineR::InternalPairwiseDistanceInferreR(Genez[,1], Genez[,2], allabsnum = length(IDs))
# Select only the top 50 (Optional, recommended for clarity)
GenIntSmall <<- G2PMineR::TopN_PickeR_Internal(GenInt, n = 50, decreasing = T)
# Assign result to distance matrix class
rwmsG <<- as.dist(GenIntSmall, diag = F, upper = FALSE)

print("Calculate internal matchwise distance matrix for P")
PhenInt <- G2PMineR::InternalPairwiseDistanceInferreR(AbsPhen$PhenoWord, AbsPhen$AbsMatches, allabsnum = length(IDs))
# Select only the top 100 (Optional, recommended for clarity)
PhenIntSmall <<- G2PMineR::TopN_PickeR_Internal(PhenInt, n = 100, decreasing = T)
# Assign result to distance matrix class
rwmsP <<- as.dist(PhenIntSmall, diag = FALSE, upper = FALSE)


print("Plot Ta internal matchwise relations network based on the distance matrix")
TaS <<- qgraph::qgraph(rwmsS, layout ="circle", labels =  gsub("_"," ",rownames(SppIntSmall)), DoNotPlot=F,label.cex=0.4)

print("Plot G internal matchwise relations network based on the distance matrix")
TaG <<- qgraph::qgraph(rwmsG, layout ="circle", labels = rownames(GenIntSmall), DoNotPlot=F,label.cex=0.4)

print("Plot P internal matchwise relations network based on the distance matrix")
TaP <<- qgraph::qgraph(rwmsP, layout ="circle", labels = rownames(PhenIntSmall),DoNotPlot=F,label.cex=0.4)
print("Sending")
            remove_modal_spinner()
            output$result9 <- renderText(
                                  {
                                    paste("Internal Network Analysis Complete")
                                  }
                                )
            require(qgraph)
            output$PlotTaS <- renderPlot(qgraph::qgraph(rwmsS, layout ="circle", labels =  gsub("_"," ",rownames(SppIntSmall)), DoNotPlot=F,label.cex=0.4))
            output$PlotTaG <- renderPlot(qgraph::qgraph(rwmsG, layout ="circle", labels = rownames(GenIntSmall), DoNotPlot=F,label.cex=0.4))
            output$PlotTaP <- renderPlot(qgraph::qgraph(rwmsP, layout ="circle", labels = rownames(PhenIntSmall),DoNotPlot=F,label.cex=0.4))
        }
                )

        PhenoGenesSmall <- NULL
        GeneSpeciesSmall <- NULL
        PhenoSpeciesSmall <- NULL
         observeEvent(input$DoBipartites,
        {
            show_modal_spinner(text = "Performing bipartite analysis, this may take a few minutes...")
            # Calculate phenotypes vs species inter-datatype matchwise distances
            PhenoSpecies <- G2PMineR::PairwiseDistanceInferreR(AbstractsSpp$Species, AbstractsSpp$Matches, AbsPhen$PhenoWord, AbsPhen$AbsMatches, allabsnum = length(IDs))
            # Select only the top 100 (Optional, recommended for clarity)
            PhenoSpeciesSmall <<- G2PMineR::TopN_PickeR(PhenoSpecies, n = 100, decreasing = T)

            # Calculate genes vs species inter-datatype matchwise distances
            GeneSpecies <- G2PMineR::PairwiseDistanceInferreR(AbstractsSpp$Species, AbstractsSpp$Matches, Genez, Genez[,2], allabsnum = length(IDs))
            # Select only the top 100 (Optional, recommended for clarity)
            GeneSpeciesSmall <<- G2PMineR::TopN_PickeR(GeneSpecies, n = 100, decreasing = T)

            # Calculate phenotypes vs genes inter-datatype matchwise distances
            PhenoGenes <- G2PMineR::PairwiseDistanceInferreR(AbsPhen$PhenoWord, AbsPhen$AbsMatches, Genez[,1], Genez[,2], allabsnum = length(IDs))
            # Select only the top 50 (Optional, recommended for clarity)
            PhenoGenesSmall <<- G2PMineR::TopN_PickeR(PhenoGenes, n = 50, decreasing = T)

            # Plot G2P internal matchwise relations network based on the distance matrix
            output$BipartitePG <- renderPlot(bipartite::plotweb(PhenoGenesSmall, text.rot=90, col.interaction = "gray",labsize = 0.75))

            # Plot Ta2G internal matchwise relations network based on the distance matrix
            output$BipartiteGS <- renderPlot(bipartite::plotweb(GeneSpeciesSmall, text.rot=90, col.interaction = "gray",labsize = 0.75))

            # Plot Ta2P internal matchwise relations network based on the distance matrix
            output$BipartitePS <- renderPlot(bipartite::plotweb(PhenoSpeciesSmall, text.rot=90, col.interaction = "gray",labsize = 0.75))
            remove_modal_spinner()
            output$result3 <- renderText(
                                  {
                                    paste("UniqueIDs Successfully Uploaded")
                                  }
                                )
        }
                )




          observeEvent(input$ExportOut,
        {
            show_modal_spinner(text = "Exporting data, this may take a few seconds...")
            tmp <- unlist(strsplit(dpath,split="\\/"))
            tmp <- unlist(strsplit("/Users/Mike/BSU_DRIVE/PhD/PhD_Projects/G2PMineRProject/G2PMineR/LICENSE",split="\\/"))
            tmp <- tmp[-length(tmp)]
            wd <- paste(tmp, collapse="/")
            write.csv(AbstractsSpp,sprintf("%s/AbstractsSpp.csv",wd),row.names=F)
            write.csv(GenesOut,"GenesOutWithSyns.csv",row.names=F)
            write.csv(GenesOut,"GenesOut.csv",row.names=F)
            write.csv(AbsPhen,"AbsPhen.csv",row.names=F)

            pdf("ConsensusVenn.pdf")
            grid.draw(ConsensusVenn)
            dev.off()

pdf(sprintf("%s/barplots.pdf",wd))
# Make Ta barplot
barplot(SppBarPlotDF[,2], names.arg = SppBarPlotDF[,1], las=2, ylim = c(0,250), ylab = "Number of Absracts")

# Make G barplot
barplot(GenesBarPlotDF[,2], names.arg = GenesBarPlotDF[,1], las=2, ylim = c(0,100), ylab = "Number of Absracts")

# Make P barplot
barplot(PhenoBarPlotDF[,2], names.arg = PhenoBarPlotDF[,1], las=2, ylim = c(0,1000), ylab = "Number of Absracts",cex.names =0.75)
dev.off()

pdf(sprintf("%s/qgraphs.pdf",wd))
# Plot Ta internal matchwise relations network based on the distance matrix
qgraph::qgraph(rwmsS, layout ="circle", labels =  gsub("_"," ",rownames(SppIntSmall)), DoNotPlot=F,label.cex=0.4)

# Plot G internal matchwise relations network based on the distance matrix
qgraph::qgraph(rwmsG, layout ="circle", labels = rownames(GenIntSmall), DoNotPlot=F,label.cex=0.4)

# Plot P internal matchwise relations network based on the distance matrix
qgraph::qgraph(rwmsP, layout ="circle", labels = rownames(PhenIntSmall),DoNotPlot=F,label.cex=0.4)
dev.off()


pdf(sprintf("%s/bipartites.pdf",wd))
# Plot G2P internal matchwise relations network based on the distance matrix
bipartite::plotweb(PhenoGenesSmall, text.rot=90, col.interaction = "gray",labsize = 0.75)

# Plot Ta2G internal matchwise relations network based on the distance matrix
bipartite::plotweb(GeneSpeciesSmall, text.rot=90, col.interaction = "gray",labsize = 0.75)

# Plot Ta2P internal matchwise relations network based on the distance matrix
bipartite::plotweb(PhenoSpeciesSmall, text.rot=90, col.interaction = "gray",labsize = 0.75)
dev.off()
remove_modal_spinner()

            output$result3 <- renderText(
                                  {
                                    paste("Output Successfully Exported")
                                  }
                                )
        }
                )

}
}
