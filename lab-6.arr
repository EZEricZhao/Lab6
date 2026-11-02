use context dcic2024
# Eric Zhao

#Task 1

#|Question 1:
   The cost of 3 lattes at £3.20 each: 3 * 3.20
   I think this should return 9.60
|#
3 * 3.20
#|Question 2:
   "Oat" + " " + "latte"
   Since there is adding a " ", then it should be "Oat Latte"
|#
"Oat" + " " + "latte"
#|Question 3:
   string-length("cappuccino")
   Since cappuccino has 10 letters, then it should return 10
|#
string-length("cappuccino")



#Task 2

#Question 1:
latte-price :: Number = 3.20
#Question 2:
fun order-cost(num-drinks :: Number, drink-price :: Number) -> Number:
  doc: "It takes the num-drinks and multiplies with drink-price to get order-cost"
  num-drinks * drink-price
where:
  order-cost(3, 1.50) is 4.50
  order-cost(15, latte-price) is 48.0
end



#Task 3

#| The café gives regular customers a discount. Design a function discount-rate that takes a number of drinks and returns:
   1. 0 for fewer than 5 drinks
   2. 0.1 for 5 to 9 drinks
   3. 0.2 for 10 or more drinks
|#

fun discount-rate(num-drinks :: Number, drink-price :: Number) -> Number:
  doc: "Similar in function to order-cost, except with the added discounts on the drinks"
  if num-drinks < 5:
    num-drinks * drink-price
  else if num-drinks <= 9:
    (num-drinks * drink-price) - (num-drinks * drink-price * 0.1)
  else:
    (num-drinks * drink-price) - (num-drinks * drink-price * 0.2)
  end
where:
  discount-rate(4, latte-price) is 12.80
  discount-rate(5, latte-price) is 14.40
  discount-rate(9, latte-price) is 25.92
  discount-rate(10, latte-price) is 25.60
end



#Task 4

orders = table: customer, drinks, price
  row: "Amira", 2, 3.20
  row: "Ben", 6, 2.80
  row: "Chen", 12, 3.00
  row: "Dara", 1, 3.50
end

#Step 1: Use build-column to add a column cost: the number of drinks times the price

build-column(orders, "cost",
  lam(r): r["drinks"] * r["price"] end)

#Step 2: Use filter-with to keep only the customers who ordered 5 or more drinks. Which customers are left?
filter-with(orders, lam(r): r["drinks"] >= 5 end)
#The only customers left are "Ben" and "Chen"



#Task 5

#Step 1: Get the drinks column of orders as a list, and call it drinks-list
drinks-list = extract drinks from orders end

#Step 2: Design total-drinks-loop, which adds up a list of numbers using a for each loop and a var
fun total-drinks-loop(lst :: List<Number>) -> Number block:
  doc: "Adds up a list of numbers using a for each loop and a var"
  var total = 0
  for each(n from lst):
    total := total + n
  end
  total
where:
  total-drinks-loop([list: ]) is 0
  total-drinks-loop([list: 1, 2, 3]) is 6
  total-drinks-loop([list: 2, 6, 12, 1]) is 21
end

#Step 3: Design total-drinks-fold, which does the same using fold
fun total-drinks-fold(lst :: List<Number>) -> Number:
  doc: "Adds up a list of numbers using fold"
  fold(lam(acc, n): acc + n end, 0, lst)
where:
  total-drinks-fold([list: ]) is 0
  total-drinks-fold([list: 1, 2, 3]) is 6
  total-drinks-fold([list: 2, 6, 12, 1]) is 21
end

#Step 4: Check that both give the same answer for drinks-list
check:
  total-drinks-loop(drinks-list) is total-drinks-fold(drinks-list)
  total-drinks-loop(drinks-list) is 21
end



#Task 6

data Order:
  | eat-in(drinks :: Number)
  | takeaway(drinks :: Number, own-cup :: Boolean)
end

#Part 1: Design a function cup-fee that takes an Order and returns the cup fee in pounds: 0.25 per drink for a takeaway without its own cup, and 0 otherwise. Use cases, and test both variants
fun cup-fee(o :: Order) -> Number:
  doc: "Returns 0.25 per drink for a takeaway without its own cup, and 0 otherwise"
  cases (Order) o: 
    | eat-in(d) => 0
    | takeaway(d, own-cup) =>
      if own-cup:
        0
      else:
        d * 0.25
      end
  end
where:
  cup-fee(eat-in(4)) is 0
  cup-fee(takeaway(4, true)) is 0
  cup-fee(takeaway(4, false)) is 1.00
  cup-fee(takeaway(0, false)) is 0
end

#Part 2: Design total-drinks-rec, which adds up a list of numbers using recursion: cases with an empty branch and a link branch. Do not use a loop or fold this time.
fun total-drinks-rec(lst :: List<Number>) -> Number:
  doc: "Adds up a list of numbers using recursion (cases with empty and link)"
  cases (List) lst:
  |empty => 0
  | link(first, rest) => first + total-drinks-rec(rest)
end
where:
  total-drinks-rec([list: ]) is 0
  total-drinks-rec([list: 5]) is 5
  total-drinks-rec([list: 1, 2, 3]) is 6
  total-drinks-rec([list: 2, 6, 12, 1]) is 21
end