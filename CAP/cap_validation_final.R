library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(yardstick)

dt <- read_excel('sample_cap_final.xlsx')

dt <- dt[, c('index_banco', 'predicted_label_final', 'policy', 'final_checagem')]
dt <- dt[dt$final_checagem != FALSE, ]

# Build the numeric-code → label lookup table as before
de_para_num <- dt %>%
  select(predicted_label_final, policy) %>%
  distinct() %>%
  arrange(predicted_label_final) %>%
  rename(code         = predicted_label_final,
         label        = policy)

# Decode the HUMAN annotation (final_checagem holds text labels,
# so we match on label to get the numeric code, then re-label)
dt <- dt %>%
  left_join(de_para_num, by = c('final_checagem' = 'label')) %>%
  rename(code_true = code)

# Now decode the MODEL prediction: join on the numeric code
# so that predicted_label_final gets its text label too
dt <- dt %>%
  left_join(de_para_num, by = c('predicted_label_final' = 'code')) %>%
  rename(label_pred = label)

# Both y_true and y_pred now carry the same text labels
# Use the same factor levels so the confusion matrix is square
all_labels <- sort(unique(de_para_num$label))

y_true <- factor(dt$final_checagem, levels = all_labels)
y_pred <- factor(dt$label_pred,     levels = all_labels)

# ── F1 scores ────────────────────────────────────────────────
metrics_df <- data.frame(truth = y_true, estimate = y_pred)

f1_macro    <- f_meas(metrics_df, truth, estimate, estimator = "macro")$.estimate
f1_micro    <- f_meas(metrics_df, truth, estimate, estimator = "micro")$.estimate
f1_weighted <- f_meas(metrics_df, truth, estimate, estimator = "macro_weighted")$.estimate

cat(sprintf("F1 Macro:    %.4f\n", f1_macro))
cat(sprintf("F1 Micro:    %.4f\n", f1_micro))
cat(sprintf("F1 Weighted: %.4f\n", f1_weighted))

# ── Confusion matrix with text labels on both axes ───────────
# all_labels is already sorted alphabetically via sort() above,
# so re-declaring factors here enforces that order on both axes
y_true <- factor(dt$final_checagem, levels = all_labels)
y_pred <- factor(dt$label_pred,     levels = all_labels)

cm_df <- as.data.frame(table(Verdadeiro = y_true, Predito = y_pred))

babel_confusionmatrix <- ggplot(cm_df, aes(x = Predito, y = Verdadeiro, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_y_discrete(limits = rev) +
  labs(
    #title = "Confusion Matrix: Babel Model Performance Across Policy Domains",
    x     = "Predicted",
    y     = "Ground Truth"
  ) +
  theme_minimal() +
  theme(
    # Rotate x labels fully vertical so nothing gets clipped
    axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y  = element_text(size = 9),
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    # Explicit white background so ggsave doesn't inherit a dark theme
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("babel_confusionmatrix.png", plot = babel_confusionmatrix,
       height = 14, width = 21, units = "cm", dpi = 700,
       bg = "white")   # belt-and-suspenders: force white background in ggsave too



