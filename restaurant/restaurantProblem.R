nb_restaurants = 5

p = runif(n = nb_restaurants, min = 0, max = 1)

generate_restaurant_rating = function(n_ratings, restaurant_index){
  rbinom(n = n_ratings, size = 5, prob = p[restaurant_index])
}

o = replicate(1000, generate_restaurant_rating(1))
barplot(table(o))
