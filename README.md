# The Jersey 12 Parish Challenge Route Generator

The channel island of [Jersey](https://www.google.co.uk/maps/place/Jersey/data=!4m2!3m1!1s0x480c52a48c927533:0x519c23a30a1a6cc3?sa=X&ved=0ahUKEwil_Lb-jYbWAhUGM8AKHTkWCvkQ8gEIlQEwEA) has [12 parishes](https://en.wikipedia.org/wiki/Parishes_of_Jersey). Unfortunately I don't live there, but my friend does, and is part of the annual 12 parish bar crawl. 

The rules are relatively simple - to drink in each of the parishes in a single day. To add an extra level of difficulty, there was a desire to travel along the bar crawl using only public transport. And to add yet another level of complexity, the bar crawl of 2017 was to be run on a Sunday, with a reduced bus service.

## Data science to the rescue?

This is akin to the [travelling salesman problem](https://simple.wikipedia.org/wiki/Travelling_salesman_problem) but with the contraints of the bus timetable, minimum drinking time, pub opening times, and no concern about how long it takes or costs, as long as it's completed within the single Sunday.

Thoughts immediately jumped to complex solutions with some intricate function that optimised comfort - 30 minutes per pub may be perfect, but 20 is acceptable, and an hour at lunch time would be great, and of course, we'd want to minimise time spent on buses, and so on. 

But the reality was we had a few days to do this and an underpowered laptop to do the calculations, so we quickly resorted to a brute force approach, which if one is being quite generous could possibly be described as some form of Monte Carlo method!

### The aim

The goals were simple

1. Find at least 1 viable route around the 12 parishes
2. Travel on using the local [Liberty bus service](https://libertybus.je)
3. Enfoce a minimum drinking time per pub

To make the problem easier, based on many years of experience, a short list of the best pubs in Jersey was provided.







