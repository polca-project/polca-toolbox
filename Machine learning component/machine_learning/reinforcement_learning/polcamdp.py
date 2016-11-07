
# Copyright (c) 2013-2016, The IMDEA Software Institute and
# Copyright (c) 2013-2016, Universidad Politecnica de Madrid

# See LICENSE.txt and AUTHORS.txt for licensing and authorship


from pybrain.rl.environments import Task
from scipy import array

class MDPPolcaTask(Task):
    """ This is a MDP task for the MazeEnvironment. The state is fully observable,
        giving the agent the current position of perseus. Reward is given on reaching
        the goal, otherwise no reward. """

    def getReward(self):
        """ compute and return the current reward (i.e. corresponding to the last action performed) """
        # if self.env.goal == self.env.perseus:
        if self.env.perseus in self.env.goal:
            reward = 1.

            # factor = 1.
            # reward = factor * (self.env.rewards[self.env.perseus])

            self.env.reset()
        else:
            reward = 0.
        return reward

    def performAction(self, action):
        """ The action vector is stripped and the only element is cast to integer and given
            to the super class.
        """

        # print("------ Action: %d ------" % (action))

        Task.performAction(self, int(action[0]))


    def getObservation(self):
        """ The agent receives its position in the maze, to make this a fully observable
            MDP problem.
        """
        # obs = array([self.env.perseus * self.env.numActions + self.env.curAction])
        # print("------ State: %d ------" % (self.env.perseus))
        obs = array([self.env.perseus])
        return obs



