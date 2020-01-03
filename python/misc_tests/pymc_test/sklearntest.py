from sklearn import datasets
from sklearn.naive_bayes import GaussianNB
from enum import Enum
import numpy as np
import IPython
import matplotlib.pyplot as plt
from itertools import repeat

# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.sklearn"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################



#Naive Bayes littering:
#initial setup
actions = Enum('actions', 'none litter clean')
actDict = { x.value - 1 : x for x in actions }
num_agents = 100
num_rounds = 300
max_initial_litter = 100
curr_litter = np.random.random_integers(0, max_initial_litter, size=(1, 2))
agent_actions = np.zeros((num_agents, 2), dtype=np.int)
rand_amnt = 0.1

def random_priors(num_classes):
    summed = 0
    while summed != 1:
        ns = np.random.random_integers(1,100,size=(num_classes))
        summed = sum(ns)
        normalized = ns / summed
        summed = sum(normalized)
    return normalized

def select_by_probability(probs):
    if np.random.random() < rand_amnt:
        chosen = np.random.choice([x for x in actions])
    else:
        chosen = np.random.choice([x for x in actions], p=probs)
    return chosen

#Classifiers: ie the brain of each agent
#initialised to have a random set of priors for behaviour
classifiers = [GaussianNB(random_priors(len(actions))) for x in agent_actions]
#History of the amount of litter there is
litter_tracking = []
action_tracking = []

#Simulate Main loop:
for x in range(num_rounds):
    logging.info("\n")
    logging.info("Turn: {}".format(x))
    logging.info("Current litter: {}".format(curr_litter[0,-1]))
    litter_tracking.append(curr_litter[0,-1])
    decisions = []
    #add a new column for amount of litter, and agent actions
    curr_litter = np.column_stack((curr_litter, curr_litter[0,-1]))
    agent_actions = np.column_stack((agent_actions, np.zeros(num_agents)))

    #Get the previous state
    prev_count_none = 1 + np.repeat(len([x for x in agent_actions[:,-3] if x == 0]), num_agents)
    prev_count_litter = 1 + np.repeat(len([x for x in agent_actions[:,-3] if x == 1]), num_agents)
    prev_count_clean = 1 + np.repeat(len([x for x in agent_actions[:,-3] if x == 2]), num_agents)
    prev_litter_amnt = 1 + np.repeat(curr_litter[:,-3], num_agents)
    prev_actions = agent_actions[:,-2]
    data = np.column_stack((prev_litter_amnt,
                            prev_count_none,
                            prev_count_litter,
                            prev_count_clean,
                            prev_actions))

    #get the current state
    curr_count_none = 1 + np.array([len([x for x in agent_actions[:,-2] if x == 0])]).reshape((-1,1))
    curr_count_litter = 1 + np.array([len([x for x in agent_actions[:,-2] if x == 1])]).reshape((-1, 1))
    curr_count_clean = 1 + np.array([len([x for x in agent_actions[:,-2] if x == 2])]).reshape((-1, 1))
    curr_litter_amnt = 1 + np.array([curr_litter[:,-2]]).reshape((-1,1))
    predict_data = np.column_stack((curr_litter_amnt,
                                    curr_count_none,
                                    curr_count_litter,
                                    curr_count_clean))

    #For Each Agent:
    for i, cls in enumerate(classifiers):
        #train on previous state
        train_data = data.copy()
        np.random.shuffle(train_data)
        amnt = int(len(train_data) * 0.5)

        cls.partial_fit(train_data[:amnt,:-1], train_data[:amnt,-1], classes=[x.value - 1 for x in actions])

        #get the current state
        #predict the probability of actions
        probs = cls.predict_proba(predict_data)[0]
        #decide based on those probabilities
        chosen = select_by_probability(probs)
        #record the decision
        decisions.append(chosen)
        agent_actions[i,-1] = chosen.value - 1
        #enact the decision
        if chosen == actions.litter:
            curr_litter[0,-1] += 1
        if chosen == actions.clean:
            curr_litter[0,-1] -= 1

    #guard against negative litter counts
    #(only necessary after all actions performed)
    if curr_litter[0,-1] < 0:
        curr_litter[0,-1] = 0

    #Log the action choices
    turn_actions = []
    for x in actions:
        amnt = decisions.count(x)
        logging.info("{} agents chose to: {}".format(amnt, x))
        turn_actions.append(amnt)

    action_tracking.append(turn_actions)
fig, axes = plt.subplots(4)


axes[0].plot(litter_tracking)
axes[1].plot([x[0] for x in action_tracking])
axes[2].plot([x[1] for x in action_tracking])
axes[3].plot([x[2] for x in action_tracking])

plt.show()
IPython.embed(simple_prompt=True)
