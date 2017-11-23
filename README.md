# market_basket
apriori - predict which product

#install.packages('arulesViz')
library(arules)
library(arulesViz)

# Load and check data
# Aisles dimension data
aisles <- read.csv("aisles.csv")
dim(aisles)
head(aisles)
# Departnemt dimaneion data
departments <- read.csv("departments.csv")
head(departments)
str(departments)
# priorders data
prior <- read.csv("order_products__prior.csv")
head(prior)
# training set data
train <- read.csv("order_products__train.csv")
head(train)
# order dimension data
orders <- read.csv("orders.csv")
head(orders)
# Product dimension data
products <- read.csv("products.csv")
head(products)
str(products)
# Submition data
ssubmit <- read.csv("sample_submission.csv")

# set seed
set.seed(12345)

# check for NAs
apply(aisles, 2, function(x) any(is.na(x)))
sum(is.na(orders$days_since_prior_order)) #orders has NAs

# merge dimensions
dimensions <- merge(x = products, y = departments, by = 'department_id', all.x = TRUE)
dimensions <- merge(x=dimensions, y = aisles, by = 'aisle_id', all.x = TRUE)
dimensions <- dimensions[,3:6]

train_spl <- train[sample(nrow(train),100000),]
train_spl <- merge(x=train_spl, y = products,by = 'product_id',all.x = TRUE)
train_spl <- subset(train_spl,select = c("order_id","product_name"))


order_ids <- c(table(table(train$order_id)))
plot(order_ids)
hist(order_ids)
summary(order_ids)

# tried transposing but didn't work
#train_spl2 <- reshape(train_spl,idvar = "order_id",timevar = "product_name", direction = "wide")
train_spl$order_id <- as.factor(train_spl$order_id)
write.csv(x = train_spl,file = "transactions.csv", quote = FALSE, row.names = FALSE)
train_trans <- read.transactions()


rules <- apriori(train_spl,parameter = list(support = 0.001, conf = 0.8))

options(digits = 2)
inspect(rules[1:4])
itemFrequencyPlot(train_spl,topN=20,type="absolute")

                        
