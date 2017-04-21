;; plan of a script to model the relationship between
;; gatekeepers, groups, expression (as a norm) and territory?

globals [
        ratio_of_gatekeepers
        food_to_agent_ratio
]

turtles-own [
            colour
            vision
            age
            max-age
            group
]

patches-own [
            owned-by
            food-value
]

;; setup procedure
to setup
   ;;init random agents around,
   ;;init unclaimed patches and a distribution of food
end

to go
   ;;move turtles
   ;;eat
   ;;form group
   ;;designate IGU members /incumbents
   ;;incumbents decide to: normalize/redistribute food, go to war, cull...
end