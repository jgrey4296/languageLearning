""" The Dry Cleaners Game from p14 of Games and Information (Rasmusen) """
import IPython
from formalgame import Game

if __name__ == "__main__":

    game = Game()
    #Add the players
    newCleaner,oldCleaner = game.addPlayer("NewCleaner","OldCleaner")
    nature = game.addPseudoPlayer('nature')[0]

    #Add the actions, minus payoffs
    enter = game.addAction(newCleaner,"Enter")
    stay = game.addAction(newCleaner,"StayOut")
    low = game.addAction(oldCleaner,"Low Price")
    high = game.addAction(oldCleaner,"High Price")
    normal_economy = game.addAction(nature,"normal")
    recession = game.addAction(nature,"recession")
    #Set the play order:
    game.setPlayerOrder(newCleaner,oldCleaner,nature)

    #Add payoffs for action profiles:
    #for normal economy
    game.addPayoff((newCleaner,enter,-100),
                   (oldCleaner,low,  -50),
                   (nature,normal_economy,0))

    game.addPayoff((newCleaner,stay,0),
                   (oldCleaner,low,50),
                   (nature,normal_economy,0))

    game.addPayoff((newCleaner,enter,100),
                   (oldCleaner,high,100),
                   (nature,normal_economy,0))

    game.addPayoff((newCleaner,stay,0),
                   (oldCleaner,high,300),
                   (nature,normal_economy,0))

    #for recession
    game.addPayoff((newCleaner,enter,-160),
                   (oldCleaner,low,  -110),
                   (nature,recession,0))

    game.addPayoff((newCleaner,stay,40),
                   (oldCleaner,low,40),
                   (nature,recession,0))

    game.addPayoff((newCleaner,enter,0),
                   (oldCleaner,high,-10),
                   (nature,recession,0))

    game.addPayoff((newCleaner,stay,0),
                   (oldCleaner,high,240),
                   (nature,recession,0))



    #Set probabilities for actions:
    game.setProbability((enter,0.5),(stay,0.5))
    game.setProbability((low,0.5),(high,0.5))

    #Todo: an alternative probability form:
    #game.setConditionalProbability(enter,low,0.2,high)
    #game.setConditionalProbability(stay,low,0.4,high)


    #Actually player the game:
    #A) specified action profiles
    payoffA,payoffB = game.play((newCleaner,enter),(oldCleaner,low),(nature,normal_economy))
    #B) randomly chosen
    r_payoffA, r_payoffB = game.play()
    #C) iterative:
    sumA,sumB = game.playIterative(n=5)
    game.reset()

    #todo Report:
    print("Report:")
    print(payoffA,payoffB)
    print(r_payoffA,r_payoffB)
    print(sumA,sumB)
    IPython.embed()
