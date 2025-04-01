#Packages
library(tidyverse)
library(patchwork)
library(gt)

so_vs_ha <- read_csv("Data/socialref_vs_hardtarg.csv")
so_vs_hu <- read_csv("Data/socsciref_vs_humantarg.csv")

sci_ana_tags <- c("SignpostingAcademicWritingMoves")
subj_ppl_tags <- c("CitationControversy")

#Remove all tags we don't care about
so_vs_ha <- filter(so_vs_ha, Tag %in% c(sci_ana_tags, subj_ppl_tags))
so_vs_hu <- filter(so_vs_hu, Tag %in% c(sci_ana_tags, subj_ppl_tags))

so_vs_ha$Tag <- factor(so_vs_ha$Tag, levels = c(subj_ppl_tags, sci_ana_tags), ordered = TRUE)
so_vs_hu$Tag <- factor(so_vs_hu$Tag, levels = c(subj_ppl_tags, sci_ana_tags), ordered = TRUE)

#Make table with RFs of the tags for each group
rf_tabl <- so_vs_ha |> select(Tag, RF, RF_Ref) |> 
  rename(ha = RF, soc = RF_Ref) |> 
  left_join(select(so_vs_hu, Tag, RF) |> rename(hu = RF),
            by="Tag")
#Plot
p1 <- ggplot(rf_tabl |> select(Tag, ha) |> rename(RF = ha)) +
  geom_col(aes(y = Tag, x = RF)) +
  geom_text(aes(y = Tag, x = RF + 0.05, label = round(RF, 2)), size=3) +
  labs(title = "Hard Sciences") +
  xlim(0,1.2) +
  theme_classic()
p2 <- ggplot(rf_tabl |> select(Tag, soc) |> rename(RF = soc)) +
  geom_col(aes(y = Tag, x = RF, fill =)) +
  geom_text(aes(y = Tag, x = RF + 0.05, label = round(RF, 2)), size=3) +
  labs(title = "Social Sciences") +
  xlim(0,1.2) +
  theme_classic()
p3 <- ggplot(rf_tabl |> select(Tag, hu) |> rename(RF = hu)) +
  geom_col(aes(y = Tag, x = RF)) +
  geom_text(aes(y = Tag, x = RF + 0.05, label = round(RF, 2)), size=3) +
  xlim(0,1.2) +
  labs(title = "Humanities") +
  theme_classic()
p1 / p2 / p3 +
  plot_layout(axis_titles = "collect")

#Boxplot for citation controversy
box_1 <- read_csv("Data/soc_vs_ha_box.csv")
box_2 <- read_csv("Data/soc_vs_hu_box.csv")[1,] |> 
  rbind(box_1)
box_2$Discipline <- factor(c("Humanities", "Hard Sciences", "Social Sciences"), levels = c("Humanities", "Social Sciences", "Hard Sciences"), ordered = TRUE)

#Boxplot for signposting
box_3 <- read_csv("Data/soc_vs_ha_box_2.csv")
box_4 <- read_csv("Data/soc_vs_hu_box_2.csv")[1,] |> 
  rbind(box_3)
box_4$Discipline <- factor(c("Humanities", "Hard Sciences", "Social Sciences"), levels = c("Humanities", "Social Sciences", "Hard Sciences"), ordered = TRUE)


p1 <- ggplot(box_2) +
  geom_boxplot(aes(x=Discipline, ymin = min, lower=`25%`, middle = `50%`, upper = `75%`, ymax = max), stat='identity') +
  labs(y = "RF", title = "Citation Controversy") +
  theme_classic()
p2 <- ggplot(box_4) +
  geom_boxplot(aes(x=Discipline, ymin = min, lower=`25%`, middle = `50%`, upper = `75%`, ymax = max), stat='identity') +
  labs(y = "RF", title = "Signposting Academic Writing Moves") +
  theme_classic()
p1 + p2 + plot_layout(axis_titles = "collect")

#Table with p-values and LL values showing the relationship between ssci and hard sci and ssci and humanities
#is significant for both tags.
tmp <- so_vs_hu |> select(Tag, LL, PV) |> mutate(LL = round(LL, 2), PV = round(PV, 3), Discipline = "Social Sciences vs. Humanities") |>
  rename("Log Likelihood" = LL, "P Value" = PV) |> 
  relocate(`Log Likelihood`, `P Value`, .after = `Discipline`)
significance_table <- rbind(
  so_vs_ha |> select(Tag, LL, PV) |> mutate(LL = round(LL, 2), PV = round(PV, 3), Discipline = "Social Sciences vs. Hard Sciences") |>
    rename("Log Likelihood" = LL, "P Value" = PV) |> 
    relocate(`Log Likelihood`, `P Value`, .after = `Discipline`),
  tmp[c(2,1),]
)
gt(significance_table, groupname_col = "Discipline", row_group_as_column = TRUE) |> 
  tab_stubhead("Relationship") |> 
  tab_style(cell_borders(sides = c("top", "bottom"), weight= px(3)), locations=list(cells_column_labels(), cells_stubhead()))
  

