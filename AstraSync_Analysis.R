# AI use (November 1, 2026): I used ChatGPT and Copilot to help generate
# and revise the calculations, simulation, plots, decision tree,
# sensitivity analysis, and dashboard code.

# I run this whole file to make the charts and dashboard.
packages_needed <- c("ggplot2", "base64enc")
packages_to_install <- packages_needed[!vapply(packages_needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(packages_to_install)) install.packages(packages_to_install, repos = "https://cloud.r-project.org")
library(ggplot2)
output_folder <- file.path(getwd(), "AstraSync_Output")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# I list each month's profit, then discount those amounts back to today.
monthly_discount <- 0.015
project_months <- 18
delivery_cost <- 1200 * 100
payment_months <- seq_len(project_months)
discount_weights <- (1 + monthly_discount)^(-payment_months)
payment_factor <- sum(discount_weights)
reward_discount <- (1 + monthly_discount)^-project_months
bonus_chance <- 0.7
award_chance <- 0.4
performance_bonus <- 800000
monthly_cashflows <- data.frame(
  Month = payment_months,
  A = rep(152000 - delivery_cost, project_months),
  B = rep(125000 - delivery_cost, project_months),
  C_if_awarded = rep(150000 - delivery_cost, project_months)
)
present_value <- function(payments) sum(payments * discount_weights)
fixed_offer_pv <- present_value(monthly_cashflows$A)
bonus_offer_base <- present_value(monthly_cashflows$B)
bonus_offer_success <- bonus_offer_base + performance_bonus * reward_discount
bonus_offer_pv <- bonus_offer_base + bonus_chance * performance_bonus * reward_discount

# I try different savings amounts, then apply the client's reward rules.
draw_savings <- function(count, minimum, likely, maximum) {
  stopifnot(count > 0, minimum < likely, likely < maximum)
  draws <- runif(count)
  lower_side <- draws <= (likely - minimum) / (maximum - minimum)
  amounts <- numeric(count)
  amounts[lower_side] <- minimum + sqrt(
    draws[lower_side] * (maximum - minimum) * (likely - minimum))
  amounts[!lower_side] <- maximum - sqrt(
    (1 - draws[!lower_side]) * (maximum - minimum) * (maximum - likely))
  amounts
}
calculate_reward <- function(amounts) {
  payout <- numeric(length(amounts))
  band_one <- amounts >= 4e6 & amounts < 6e6
  band_two <- amounts >= 6e6 & amounts <= 8e6
  band_three <- amounts > 8e6
  payout[band_one] <- 0.20 * (amounts[band_one] - 4e6)
  payout[band_two] <- 400000 + 0.40 * (amounts[band_two] - 6e6)
  payout[band_three] <- 1200000 + 0.60 * (amounts[band_three] - 8e6)
  payout
}
set.seed(2026)
trial_count <- 1000000L
client_savings <- draw_savings(trial_count, 3.2e6, 5.6e6, 12.8e6)
rewards <- calculate_reward(client_savings)
average_reward <- mean(rewards)
middle_reward <- median(rewards)
bid_base_pv <- present_value(monthly_cashflows$C_if_awarded)
awarded_bid_pv <- bid_base_pv + average_reward * reward_discount
competitive_bid_pv <- award_chance * awarded_bid_pv
simulation_error <- award_chance * reward_discount * sd(rewards) / sqrt(trial_count)
stopifnot(
  abs(fixed_offer_pv - (152000 - delivery_cost) *
        (1 - (1 + monthly_discount)^(-project_months)) / monthly_discount) < 0.01,
  abs(bonus_offer_pv - (bonus_chance * bonus_offer_success +
        (1 - bonus_chance) * bonus_offer_base)) < 0.01,
  all(calculate_reward(c(3.2e6, 4e6, 6e6, 8e6, 12.8e6)) ==
        c(0, 0, 400000, 1200000, 4080000))
)

dollars <- function(amount) {
  sprintf("$%s", format(round(amount, 2), nsmall = 2, big.mark = ",", trim = TRUE))
}
percent_label <- function(chance) sprintf("%.2f%%", chance * 100)
offer_summary <- data.frame(Option = c("A", "B", "C"),
                     Expected_PV = c(fixed_offer_pv, bonus_offer_pv, competitive_bid_pv))


write.csv(monthly_cashflows, file.path(output_folder, "Monthly_Cashflows.csv"), row.names = FALSE)
write.csv(offer_summary, file.path(output_folder, "Expected_Payoffs.csv"), row.names = FALSE)
print(offer_summary)
cat("Simulated mean gain-share:", dollars(average_reward), "\n")
cat("Simulated median gain-share:", dollars(middle_reward), "\n")
cat("Monte Carlo standard error of option C:", dollars(simulation_error), "\n")

chart_style <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", colour = "#142b3b"),
        panel.grid.minor = element_blank(), legend.position = "bottom")
reward_markers <- data.frame(
  value = c(average_reward, middle_reward) / 1e6,
  statistic = factor(c("Mean", "Median"), levels = c("Mean", "Median"))
)
reward_histogram <- ggplot(data.frame(Share = rewards), aes(Share / 1e6)) +
  geom_histogram(binwidth = 0.1, boundary = 0, fill = "#147d92", colour = "white") +
  geom_vline(data = reward_markers,
             aes(xintercept = value, colour = statistic, linetype = statistic),
             linewidth = 1) +
  scale_colour_manual(name = NULL, values = c(Mean = "#c64d21", Median = "#572c85"),
                      labels = c(paste("Mean:", dollars(average_reward)),
                                 paste("Median:", dollars(middle_reward)))) +
  scale_linetype_manual(name = NULL, values = c(Mean = "solid", Median = "dashed"),
                        labels = c(paste("Mean:", dollars(average_reward)),
                                   paste("Median:", dollars(middle_reward)))) +
  labs(title = "Option C: simulated gain-share reward",
       subtitle = paste(format(trial_count, big.mark = ","),
                        "triangular simulations | Conditional on winning the RFP"),
       x = "Gain-share paid at month 18 ($ millions)", y = "Simulation count") + chart_style

# I show the choices and average payoffs in the tree.
tree_edges <- data.frame(x = c(.6,.6,.6,3.45,3.45,3.45,3.45),
                       y = c(4,4,4,4,4,1.6,1.6),
                       xend = c(6.55,3.45,3.45,6.55,6.55,6.55,6.55),
                       yend = c(6.4,4,1.6,4.8,3.3,2.1,.5))
branch_labels <- data.frame(x = c(3.5,1.95,1.95,4.9,4.9,4.9,4.9),
                     y = c(5.55,4.35,2.55,4.65,3.4,2.05,.65),
                     text = c(paste0("A: accept fixed price\nEV = ",dollars(fixed_offer_pv)),
                       paste0("B: accept bonus offer\nEV = ",dollars(bonus_offer_pv)),
                       paste0("C: bid $150,000/month\nEV = ",dollars(competitive_bid_pv)),
                       "Bonus earned: p = 0.70", "No bonus: p = 0.30",
                       "Win RFP: p = 0.40", "Lose RFP: p = 0.60"))
outcome_labels <- data.frame(x = 7.65, y = c(6.4,4.8,3.3,2.1,.5),
  text = c(paste0("Certain PV\n",dollars(fixed_offer_pv)),
           paste0("PV with bonus\n",dollars(bonus_offer_success)),
           paste0("PV without bonus\n",dollars(bonus_offer_base)),
           paste0("Conditional expected PV\n",dollars(awarded_bid_pv),
                  "\nSavings ~ Tri(3.2M, 5.6M, 12.8M)\nMean share = ",dollars(average_reward)),
           "No contract\nPV = $0"))
contract_tree <- ggplot() +
  geom_segment(data = tree_edges, aes(x=x,y=y,xend=xend,yend=yend), colour="#637785", linewidth=.7) +
  geom_point(aes(x=.6,y=4), shape=22, size=13, fill="#142b3b", colour="#142b3b") +
  geom_point(data=data.frame(x=3.45,y=c(4,1.6)), aes(x,y), shape=21, size=10,
             fill="#d8f1ef", colour="#147d92") +
  geom_label(data=branch_labels, aes(x,y,label=text), size=3.4, fill="white", colour="#142b3b",
             label.size=0, lineheight=1.1) +
  geom_label(data=outcome_labels, aes(x,y,label=text), size=3.4, fill="#edf5f6", colour="#142b3b",
             label.size=.25, lineheight=1.15) +
  annotate("text",x=.6,y=3.35,label="Choose\ncontract",size=3.4) +
  coord_cartesian(xlim=c(0,9.4),ylim=c(0,7),clip="off") + theme_void() +
  labs(title="AstraSync contract decision tree",
       subtitle=paste0("Expected net contributions in present dollars | Monthly discount rate: 1.5% | Best EV: ",
                       offer_summary$Option[which.max(offer_summary$Expected_PV)]),
       caption="Square: decision | Circles: chance | C win payoff includes the average simulated gain-share.\nImplementation costs apply only when a contract is awarded; no bidding costs are specified.") +
  theme(plot.title=element_text(size=19,face="bold",colour="#142b3b"),
        plot.subtitle=element_text(size=12),plot.caption=element_text(size=10,hjust=0),
        plot.margin=margin(20,20,20,20))

# I change one probability at a time to see when the best choice changes.
chance_values <- seq(0,1,length.out=501)
probability_results <- rbind(
  data.frame(Input="Bonus probability (B)", Probability=rep(chance_values,3),
             Option=rep(c("A","B","C"),each=length(chance_values)),
             PV=c(rep(fixed_offer_pv,length(chance_values)),bonus_offer_base+chance_values*performance_bonus*reward_discount,
                  rep(competitive_bid_pv,length(chance_values)))),
  data.frame(Input="RFP win probability (C)",Probability=rep(chance_values,3),
             Option=rep(c("A","B","C"),each=length(chance_values)),
             PV=c(rep(fixed_offer_pv,length(chance_values)),rep(bonus_offer_pv,length(chance_values)),chance_values*awarded_bid_pv)))
bonus_break_even <- (fixed_offer_pv-bonus_offer_base)/(performance_bonus*reward_discount)
bonus_winning_threshold <- (max(fixed_offer_pv,competitive_bid_pv)-bonus_offer_base)/(performance_bonus*reward_discount)
award_winning_threshold <- max(fixed_offer_pv,bonus_offer_pv)/awarded_bid_pv
case_probabilities <- data.frame(Input=c("Bonus probability (B)","RFP win probability (C)"),
                      Probability=c(bonus_chance,award_chance))
probability_chart <- ggplot(probability_results,aes(Probability,PV/1000,colour=Option)) +
  geom_line(linewidth=1) +
  geom_vline(data=case_probabilities,aes(xintercept=Probability),
             linetype="dotted",colour="#45515a") +
  facet_wrap(~Input,ncol=2) +
  scale_colour_manual(values=c(A="#64748b",B="#c64d21",C="#147d92")) +
  scale_x_continuous(labels =function(x) paste0(round(x*100),"%")) +
  labs(title="Which probability changes the preferred contract?",
       subtitle="One input changes at a time; dotted lines show current assumptions",
       x="Probability",y="Expected present value ($ thousands)") + chart_style

for (item in c("Histogram","Decision_Tree","Sensitivity")) {
  p <- switch(item,Histogram=reward_histogram,Decision_Tree=contract_tree,Sensitivity=probability_chart)
  ggsave(file.path(output_folder,paste0(item,".png")),p,
         width=if(item=="Decision_Tree") 16 else 12,
         height=if(item=="Decision_Tree") 9 else 6.5,dpi=180,bg="white")
}

# I put the results and charts in one page that opens in a browser.
embed_chart <- function(name) paste0('<img alt="',name,'" src="data:image/png;base64,',
  base64enc::base64encode(file.path(output_folder,paste0(name,".png"))),'">')
preferred_offer <- offer_summary$Option[which.max(offer_summary$Expected_PV)]
overview_text <- paste0("AstraSync must choose a contract that covers its $120,000 monthly delivery cost while balancing profit and uncertainty. ",
  "I discount 18 months of profit at 1.5% per month and simulate ",format(trial_count,big.mark=","),
  " triangular client-savings outcomes for the RFP gain-share. ",
  "Expected present values are ",dollars(fixed_offer_pv)," for A, ",dollars(bonus_offer_pv)," for B, and ",dollars(competitive_bid_pv)," for C. ",
  "I recommend option ",preferred_offer," because it has the highest average payoff. ",
  "Its small advantage depends on the 40% chance of winning, with a 60% chance of no contract.")
dashboard_page <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
 '<title>AstraSync | Contract Decision</title><style>',
 'body{margin:0;background:#f1f4f6;color:#182e3c;font:17px/1.65 system-ui,sans-serif}header{background:#132c3d;color:white;padding:48px max(6vw,20px)}header p{color:#b8dce2}h1{font-size:clamp(32px,5vw,56px);line-height:1.1;margin:12px 0}nav{display:flex;gap:20px;flex-wrap:wrap}nav a{color:#b8dce2}main{max-width:1180px;margin:auto;padding:25px}section{background:white;padding:28px 32px;margin:20px 0;border-radius:8px}h2{font-size:28px;margin-top:0}img{width:100%;height:auto}table{border-collapse:collapse;width:100%}td,th{text-align:left;padding:14px;border-bottom:1px solid #d6e1e5}th{background:#edf5f6}.recommend{border-left:5px solid #147d92}.note{font-size:14px;color:#526874}@media(max-width:650px){section{padding:20px}main{padding:10px}table{font-size:14px}}',
 '</style></head><body><header><p>ASTRASYNC / DECISION ANALYSIS</p><h1>AstraSync Contract Analysis</h1>',
 '<nav><a href="#summary">Summary</a><a href="#analysis">Analysis</a><a href="#illustration">Illustration</a><a href="#sensitivity">Sensitivity</a><a href="#conclusion">Conclusion</a></nav></header><main>',
 '<section id="summary" class="recommend"><h2>Summary</h2><p>',overview_text,'</p></section>',
 '<section id="analysis"><h2>Analysis</h2><table><tr><th>Alternative</th><th>Contract terms and calculation</th><th>Expected PV</th></tr>',
 '<tr><td>A: Fixed payment</td><td>$152,000 monthly; certain $32,000 monthly contribution for 18 months.</td><td>',dollars(fixed_offer_pv),'</td></tr>',
 '<tr><td>B: Performance bonus</td><td>$125,000 monthly; $5,000 monthly contribution plus an $800,000 month-18 bonus with 70% probability.</td><td>',dollars(bonus_offer_pv),'</td></tr>',
 '<tr><td>C: Competitive RFP</td><td>Bid $150,000 monthly; 40% chance of winning. If awarded, $30,000 monthly contribution plus a variable month-18 gain-share. Otherwise $0.</td><td>',dollars(competitive_bid_pv),'</td></tr></table>',
 '<p>Annuity factor = ',round(payment_factor,6),'; month-18 discount factor = ',round(reward_discount,6),'. A = 32,000 × annuity factor. B = 5,000 × annuity factor + 0.70 × 800,000 × terminal factor. C = 0.40 × [30,000 × annuity factor + simulated mean gain-share × terminal factor].</p>',
 '<h3>Client savings and gain-share</h3><p>Savings follow Triangular(low $3.2M, mode $5.6M, high $12.8M), conditional on winning. The share is $0 below $4M; 20% of excess above $4M through $6M; $400,000 + 40% of excess above $6M through $8M; and $1.2M + 60% of excess above $8M beyond $8M.</p>',
 embed_chart("Histogram"),'<p>Mean reward: ',dollars(average_reward),'; median: ',dollars(middle_reward),'. The right tail pulls the mean above the median. These are undiscounted rewards conditional on winning, rather than total contract payoffs. Monte Carlo standard error for C: ',dollars(simulation_error),'.</p></section>',
 '<section id="illustration"><h2>Illustration</h2>',embed_chart("Decision_Tree"),'</section>',
 '<section id="sensitivity"><h2>Sensitivity analysis</h2><p>Bonus probability and RFP win probability directly control the expected value of the uncertain offers. The charts vary each from 0% to 100% while holding all other inputs constant.</p>',embed_chart("Sensitivity"),
 '<p><strong>Bonus probability:</strong> B beats A above ',percent_label(bonus_break_even),' and becomes the best of all three above ',percent_label(bonus_winning_threshold),', holding the RFP win probability at 40%.</p>',
 '<p><strong>RFP win probability:</strong> C becomes the best option above ',percent_label(award_winning_threshold),', holding the bonus probability at 70%. The current 40% estimate is only slightly above this threshold.</p></section>',
 '<section id="conclusion" class="recommend"><h2>Conclusion</h2><p>I would choose ',preferred_offer,' based on its average payoff. Its lead over B is ',dollars(competitive_bid_pv-bonus_offer_pv),', while losing the RFP produces no contract with 60% probability. If AstraSync prioritizes a guaranteed contribution, A offers ',dollars(fixed_offer_pv),' with no bonus or award uncertainty. Validate the win estimate and savings forecast before making the final commercial decision.</p>',
 '<p class="note">Assumptions: month-end operating costs and payments; gain-share at month 18; no unprovided bid costs, taxes, or opportunity costs; delivery cost fixed at $120,000/month; RFP savings are conditional on award. Sampling variability causes C to differ slightly across seeds and simulation counts.</p></section><footer class="note" style="padding:20px"><strong>AI use, November 1, 2026:</strong> I used ChatGPT and Copilot to help generate and revise the calculations, simulation, plots, decision tree, and sensitivity analysis, and to help finish the dashboard.</footer></main></body></html>')
writeLines(enc2utf8(dashboard_page),file.path(output_folder,"AstraSync_Dashboard.html"),useBytes=TRUE)
writeLines(enc2utf8(dashboard_page),file.path(output_folder,"index.html"),useBytes=TRUE)
cat("\nFiles saved in:",normalizePath(output_folder),"\n")
if (interactive()) browseURL(file.path(output_folder,"AstraSync_Dashboard.html"))
