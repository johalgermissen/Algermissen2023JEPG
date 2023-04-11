#!/usr/bin/env python2
# -*- coding: utf-8 -*-
__prj__ = 'Oyster Farming Task--Study 2'
__license__ = 'See attached licence'
__author__ = 'Johannes Algermissen'
__email__ = 'j.algermissen@donders.ru.nl'
__date__ = '2021/09/15'

# https://tsgdoc.socsci.ru.nl/index.php?title=ButtonBoxes
# https://stackoverflow.com/questions/35071433/psychopy-and-pylink-example

###########################################################################
# Load modules
###########################################################################

# Eye-tracking modules:
from pylink import *
import pylink as pylink
import time
import gc
import sys
import os

import numpy # to retrieve screen size
import math # for sqrt

# For button box:
import serial # for button box

# Other modules needed for task:
import os.path # for absolute path

# Psychopy:
from psychopy import core, event, visual, data, gui

import random

from psychopy import logging
#logging.console.setLevel(logging.WARNING)
# https://discourse.psychopy.org/t/turning-off-all-logging/4575

###########################################################################
# Request participant number, gender, and age via GUI
###########################################################################

def function_gui_info():
    guiDict = {"1. Subject": 1,"2. Age": 1,"3. Gender":["male","female","other"],"4. Hand":["left_hand","right_hand"],"5. Eye":["left_eye","right_eye"],
    "6. Version":["1: Full","2: Demo"],"7. Use button box":["1: Yes","2: No"],"8. Use eye-tracker":["1: Yes","2: No"],"9. Use gaze-contingent":["1: Yes","2: No"],
    "A. Skip practice blocks":["1: No","2: Yes"],"B. nRep": 22} # define dictionary with infos to be asked for
    dialog = gui.DlgFromDict(dictionary = guiDict, title = "Go/NoGo Task", fixed = ("options")) # ask for infos
    
    # if info received: define variables
    if dialog.OK:
        subject = int(guiDict["1. Subject"])
        age = int(guiDict["2. Age"])
        gender = guiDict["3. Gender"]
        hand = guiDict["4. Hand"]
        eye = guiDict["5. Eye"]
        
        if guiDict["6. Version"] == "1: Full": # use more instructions, 1 instead of 3 blocks
            isFull = True
        else: 
            isFull = False
            
        if guiDict["7. Use button box"] == "1: Yes": # use button box or keyboard
            useButtonBox = True
            print('Use button box')
        else: 
            useButtonBox = False
            
        if guiDict["8. Use eye-tracker"] == "1: Yes": # use eye-tracker or not
            useEyeTracker = True
            print('Use eye-tracker')
        else: 
            useEyeTracker = False
            
        if guiDict["9. Use gaze-contingent"] == "1: Yes": # use gaze-contingent changes or not
            useGazeContingent = True
        else: 
            useGazeContingent = False
            
        if (not useEyeTracker) and useGazeContingent:
            print('Cannot use gaze-contingent feature without eye-tracker---abort')
            core.quit()
            
        if guiDict["A. Skip practice blocks"] == "2: Yes": # use gaze-contingent changes or not
            skipPractice = True
        else: 
            skipPractice = False

        nTrials = int(guiDict["B. nRep"]) * nCues * nSes; # number of cue repetitions per block: 
#        nTrials = 20
        # Print settings to console:
        print('useButtonBox is {}, useEyeTracker is {}, useGazeContingent is {}'.format(str(useButtonBox),str(useEyeTracker),str(useGazeContingent)))

        # Check whether input file exists:
        exists = os.path.isfile("TrialHandler/stimuluslist_test_stimNum_{}_sub_{:0>3d}_block1.csv".format(nTrials,subject))        
        if not exists:
            print(u"Stop -- trialHandler file for subject {} with {} trials does not exist".format(subject,nTrials))
            core.quit()

        return (subject, age, gender, hand, eye, isFull, useButtonBox, useEyeTracker, useGazeContingent, skipPractice, nTrials)
    else: # otherwise quit experiment and display reason
        print("Stop -- missing participant information")
        core.quit()

###########################################################################
# Abort task while properly closing eye-tracker
###########################################################################

def abort():
    print('Pressed key to abort task')
    if useEyeTracker:
        sentence_text.setText(u'Aborted, closing eye-tracking file...')
        sentence_text.draw()
        win.flip()
        getEYELINK().sendMessage("EXPERIMENT ABORTED")
        EYELINK.stopRecording();
        EYELINK.setOfflineMode();
        msecDelay(500); # in ms
        #Close the file and transfer it to Display PC
        EYELINK.closeDataFile()
        EYELINK.receiveDataFile(edfFileName, edfFileName)
    core.quit()

###########################################################################
# Wait for button press
###########################################################################

def waitforbutton(waitTime = 3, maxWait = 10, waitTillEnd = False):
    # total presentation time is waitTime (guaranteed) + maxWait (optional maximum)

    # Initialize to NA:
    response = 0 # NoGo by default
    respSide = 'NA' # only gets defined for Go
    RT = 'NA' # only gets defined for Go
    
    # 1) Wait minimum time before enabling responses:
    core.wait(waitTime) # make sure that participants see instructions for a certain minimal amount of time, and do not just click through it      
    startTime = core.getTime() #  Get start time
    
    ############################################################################
    # 2a) if button box response:
    if useButtonBox:
        
        bb.read(bb.inWaiting()) # read/ delete any bytes from bb
        cnt = bb.inWaiting() # initialize cnt object
        notDone = True
        
        while notDone: # wait for time expiration or button press (change in bytes) as long as needed
            
            latency = core.getTime() - startTime # record latency immediately
            
            if latency > maxWait: # if latency exceeds maximum wait
                notDone = False # exit after NoGo in next while loop
            
            if bb.inWaiting() > cnt: # if any button pressed
                
                response = 1
                notDone = False # exit after Go in next while loop
                
                button = bb.read(bb.inWaiting()) # read button press
                button = str(button,'utf-8') # Turn into string
                if isPrint:
                    print('Pressed key is {}'.format(button))
                                
                if 'H' in button or 'h' in button: # if abort:
                    print('Pressed H, abort')
                    abort()
                    
                elif 'E' in button or 'e' in button: # if left Go
                    respSide = 1
                    if isPrint:
                        print(u'Left Go')    
                    
                elif 'A' in button or 'a' in button: # if right Go
                    respSide = 0
                    if isPrint:
                        print(u'Right Go')
                
                else: 
                    print('Pressed unknown key, abort')
                    abort()
                
                RT = round(latency,3)
                if isPrint:
                    print('RT is {:.3f}'.format(RT))

    ############################################################################
    # 2b) if keyboard response:
    else:
        
        keyPress = event.waitKeys(keyList = ['v','b','escape'], maxWait = maxWait) # allow people to press buttons until maxWait, otherwise advance
        
        if keyPress: # once key press recorded:
            
            latency = core.getTime() - startTime # record latency immediately

            response = 1
            
            button = keyPress[0] # key pressed
            button = str(button) # turn into string
            if isPrint:
                print('Pressed key is {}'.format(button))
                        
            if keyPress[0] == 'escape': # If abort:
                print('Pressed escape, abort')
                abort()
                
            elif button == 'v': # if left Go
                respSide = 1
                if isPrint:
                    print(u'Left Go')
                
            elif button == 'b': # if right Go
                respSide = 0
                if isPrint:
                    print(u'Right Go')           
                
            else: 
                print('Pressed unknown key, abort')
                abort()
            
            RT = round(latency,3)
            if isPrint:
                print('RT is {:.3f}'.format(RT))

    ############################################################################
    # 3) If response made and chosen to wait till end:
    
    if response == 1 and waitTillEnd: # if chosen to wait till end of trial
        endWait = round(maxWait-latency,3)
        if isPrint:
            print('Wait {} seconds till end'.format(str(endWait)))
        core.wait(endWait) # wait until end
        
    # Return response, key-side, RT
    return (response,respSide,RT)

###########################################################################
# Overall function displaying slide of instructions
###########################################################################

def function_display_instructions(file, waitTime = 1.0, maxWait = 200, waitTillEnd = False, waitAtEnd = 0.3):

    # Draw instructions:
    instr_image.setImage(file) # retrieve new image
    instr_image.draw()
    win.flip()
    
    # Wait for button press:
    waitforbutton(waitTime = waitTime, maxWait = maxWait, waitTillEnd = waitTillEnd) # auto-advance after 10 seconds
#    win.flip() # clean screen
    
    # Wait at the end for smoother transition:
    core.wait(waitAtEnd) # wait for 300 ms until next screen follows (avoid rapid transitions)
    
###########################################################################
# Display countdown at beginning of each block
###########################################################################

def function_countdown(): # countdownPresent as global variable
    for i in ["3","2","1","Start!"]:
        center_fix.setText(i)
        center_fix.draw()
        win.flip()  
        waitforbutton(waitTime = 0.05, maxWait = countdownPresent - 0.05, waitTillEnd = True) # allow for 0.95 seconds to press abort

###########################################################################
# Run trials main task -- either for practice or for test
###########################################################################

def function_run_main_trials(trials, blockType, blockNr, testGazeContingent = False):

    ###########################################################
    # Intialize performance statistics

    countTrial = 0
    countValidPoints = 0
    countPoints = 0
    countCatch = 0 # counts of stakes recognition
    countCatchCorrect = 0 # correct responses in stakes recognition
    
    ###########################################################
     # Display countdown
    function_countdown()
    
    ###########################################################
    # Continue recording
    if useEyeTracker and ((blockType == 'Test') or testGazeContingent): # if eyetracker used and either Test recording or gaze contingency needed: start eye tracker
        error = EYELINK.startRecording(1,1,1,1) # 
        if error: core.quit() #return error;    
        pylink.beginRealTimeMode(100)
        getEYELINK().sendCommand("link_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,BUTTON,INPUT")
        getEYELINK().sendCommand("link_sample_data  = LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS,HTARGET,INPUT")    

    ###########################################################
    # Run through trials
    for trial in trials:

        ###########################################################
        ## I) Retrieve picture and answers
        
        sessionNr       = trial['sessionNr'] # either retrieve or count yourself
        trialNr         = trial['trialNr'] # either retrieve or count yourself
        
        isActionCue     = trial['isActionCue'] # either retrieve or count yourself
        isStakes        = trial['isStakes'] # either retrieve or count yourself
        isReleaseCue    = trial['isReleaseCue'] # either retrieve or count yourself
        isOutcome       = trial['isOutcome'] # either retrieve or count yourself

        cue             = trial['cue'] # name of .png file
        reqAction       = trial['reqAction'] # whether Go(1) or NoGo (0) required
        reqSide         = trial['reqSide'] # whether release cue is left(1) or right (0)
        rewLeft         = trial['rewLeft'] # reward on the left (1) or right (0)
        angle           = trial['angle'] # angle of upwards/downwards displacement
        rewMag          = trial['rewMag'] # reward stakes
        punMag          = trial['punMag'] # punishment stakes
        goValidity      = trial['goValidity'] # give valid or invalid feedback given Go response
        nogoValidity    = trial['nogoValidity'] # give valid or invalid feedback given NoGo response
        isCatch         = trial['isCatch'] # whether catch trial (1) or not (0)
        ISI             = trial['ISI']
        ITI             = trial['ITI']

        # Prepare stakes images:
        rewStakes = 'TaskStimuli/StakesCues/{}_pearl_{}.png'.format(rewColor,rewMag)
        punStakes = 'TaskStimuli/StakesCues/{}_tumor_{}.png'.format(punColor,punMag)
        
        # Count trials and rewards for determining final payoff:
        countTrial += 1

        # Print that new trial starts to console:
        print(u'####################################################################')
        print(u'Trialnr is {}'. format(str(trialNr)))
        if isPrint:
            print(u'Required action is {}'. format(str(reqAction)))
            print(u'Required side is {}'. format(str(reqSide)))
            print(u'Rew magnitude is {}'. format(str(rewMag)))
            print(u'Pun magnitude is {}'. format(str(punMag)))
        
        ###########################################################
        ## II) Intertrial interval (ITI): 1200-2800 ms
        
        isITI = True
        if isITI:
            center_fix.setText('+')
            center_fix.draw()
            win.flip()
            waitforbutton(waitTime = 0.05, maxWait = ITIintercept+ITI-0.050, waitTillEnd = True) # allow for last 95% to abort; wait until the end
    #        win.flip()
        
        ###########################################################
        ## III) Present action cue:

        # Initialize and draw cue:
        if isActionCue:
            
            # Draw oyster in the background:
            if isStakes: # draw already stakes mappin on background
                if rewLeft == 1:
                    instr_image.setImage(backgroundRewLeft) # rewards on the left
                else:
                    instr_image.setImage(backgroundRewRight) # rewards on the right
            else:
                instr_image.setImage('Instructions/Oyster.png')
            instr_image.draw()
            
            # Draw cue in the foreground:
            center_image.setImage('TaskStimuli/ActionCues/{}.png'.format(cue)) # retrieve cue 
            center_image.draw() # then draw cue
            win.flip() # flip only after potential outcomes presented?

            # Start message to eye-tracker:
            if useEyeTracker and (blockType == 'Test'):
                getEYELINK().sendMessage("StartActionCue")
                
            # Wait:
            core.wait(actionCuePresent) # present

            # Stop message to eye-tracker:
            if useEyeTracker and (blockType == 'Test'):
                getEYELINK().sendMessage("StopActionCue")
                
            # Fixation cross on screen:
#            instr_image.draw() # draw oyster in the background again
#            center_fix.setText('+')
#            center_fix.draw()
#            win.flip()
#            core.wait(0.2)
                    
        ###########################################################
        ## IV) Present stakes:
        
        if isStakes:
            
            # Compute angle to radians:
            if isPrint:
                print('Angle = {} degrees'.format(angle))
            rad_angle = angle*(math.pi/180) # radians(angle)

            # Number for rounding (precision in spatial position):
            nRounding = 3

            # Compute coordinates of images on screen (relative to screen size):
            left_x_pos  = -1 * round(math.cos(rad_angle)*eccentricity,nRounding) # left side: negative
            left_y_pos  =  elevation + round(math.sin(rad_angle)*eccentricity,nRounding) 
            right_x_pos = round(math.cos(rad_angle)*eccentricity,nRounding) # right side: positive
            right_y_pos =  elevation + 1 * round(math.sin(rad_angle)*eccentricity,nRounding) # same height (+1) or opposite height (-1)?

            # Move stakes images:
            left_image.setPos((left_x_pos,left_y_pos)) #
            right_image.setPos((right_x_pos,right_y_pos)) #

            # Transform into absolute pixels to compare to gaze position on screen:
            leftROI     = [scrx/2 + (scrx/2)*left_x_pos, scry/2 - (scry/2)*left_y_pos] # concatenate in vector
            rightROI    = [scrx/2 + (scrx/2)*right_x_pos, scry/2 - (scry/2)*right_y_pos] # concatenate in vector
                        
            # if isPrint:
            if isPrint:
                print('Middle of screen: x = {}; y = {}'.format(scrx/2, scry/2))
                print('Left stakes: x = {}, y = {}; Right stakes: x = {}, y = {}'.format(leftROI[0],leftROI[1],rightROI[0],rightROI[1]))

            # Define left/ right text as reward/ punishment stakes:
            if rewLeft == 1: # if rewards presented on the left
                leftStakes = rewStakes
                rightStakes = punStakes
                leftFuzzy = rewFuzzy
                rightFuzzy = punFuzzy
                    
            else: # if rewards presented on the right
                leftStakes = punStakes
                rightStakes = rewStakes
                leftFuzzy = punFuzzy
                rightFuzzy = rewFuzzy
                
            if testGazeContingent: # if gaze-contingent: first draw fuzzy placeholders
                left_image.setImage(leftFuzzy)
                right_image.setImage(rightFuzzy)
                
            else: # draw actual stakes
                left_image.setImage(leftStakes)
                right_image.setImage(rightStakes)
                
            # Draw oyster in the background again (already loaded):
#            instr_image.setImage('Instructions/Oyster.png')
#            instr_image.setImage(backgroundRewLeft) # rewards on the left
#            instr_image.setImage(backgroundRewRight) # rewards on the right
            instr_image.draw()

            # Draw both stakes in the foreground: 
            left_image.draw() # then draw cue
            right_image.draw() # then draw cue
            win.flip() 
            
            # Start message to eye-tracker:
            if useEyeTracker and (blockType == 'Test'):
                getEYELINK().sendMessage("StartStakes")
                        
            # Timing:
            if testGazeContingent:
                
                # Time Settings:
                
                checkInterval = 0.020 # 20 ms    
                    
                # Initialize variables:
                notDone = True
                startTime = core.getTime() #  get starting time
                checkTime = startTime # for checking whether eye-tracker should be checked
                        
                while notDone:
                    
                    # Get latency:
                    latency = core.getTime() - startTime # compute latency
                    
                    # If longer than maximum: exit
                    if latency > stakesPresent:
                        notDone = False
                        
                    # If interval reached: change opacity:
                    if (core.getTime()-checkTime) > checkInterval: # if enough time passed since last checkTime:
                
                        # Reset checktime:
                        checkTime = core.getTime()
                #        print('Time is {:1.3f}'.format(latency))
                                
                        ####################################################################
                        ## Gaze contingency:
                        
                        if testGazeContingent:
                                                    
                            eventType=eyelink.getNextData() # Retrieve sample data from eye-tracker:
#                            print('New gaze update detected at latency {}: ########'.format(str(latency)))                                
                            sample=eyelink.getNewestSample() # retrieve sample from eyelink
                            
                            if sample != None: # if no sample defined
                                
                                ## i) If sample not defined: retrieve samples, calculate position on screen
                                
                                if sample.isLeftSample(): # retrieve left eye
                                    gazePos = sample.getLeftEye().getGaze()

                                if sample.isRightSample(): # retrieve right eye
                                    gazePos = sample.getRightEye().getGaze()
                                
                                if isPrint:
                                    print('Gaze position" x = {}, y = {}'.format(str(round(gazePos[0],2)),str(round(gazePos[1],2))))
                                    
                                ## ii) Left stakes:

                                eucDistFix = math.sqrt((gazePos[0]-leftROI[0])**2+(gazePos[1]-leftROI[1])**2) # Pythagoras
                                if isPrint:
                                    print('eucDistFix to left cue is {}'.format(str(eucDistFix)))
                                
                                if eucDistFix < tolFix: # in units of pixels 
                                    left_image.setImage(leftStakes) # reveal outcome
                                    if isPrint:
                                        print('Reveal left outcome')
                                else:
                                    left_image.setImage(leftFuzzy) # conceil outcome
                                    if isPrint:
                                        print('Conceal left outcome')

                                ## iii) Right stakes:
                                
                                eucDistFix = math.sqrt((gazePos[0]-rightROI[0])**2+(gazePos[1]-rightROI[1])**2) # Pythagoras
                                if isPrint:
                                    print('eucDistFix to right cue is {}'.format(str(eucDistFix)))
                                
                                if eucDistFix < tolFix: # in units of pixels
                                    right_image.setImage(rightStakes) # reveal outcome
                                    if isPrint:
                                        print('Reveal right outcome')
                                else:
                                    right_image.setImage(rightFuzzy) # conceil outcome
                                    if isPrint:
                                        print('Conceal right outcome')
                                    
                                eyelink.resetData() # does this delete anything in the eyelink recorded data? Is this necessary?
                                                        
                                ### Draw again:
                                
                                instr_image.draw() # draw oyster again
                                left_image.draw() # draw left stakes again
                                right_image.draw() # draw right stakes again
                                win.flip() 
                                
            else: # otherwise just wait passively
                core.wait(stakesPresent)
        
            # Stop message to eye-tracker:
            if useEyeTracker and (blockType == 'Test'):
                getEYELINK().sendMessage("StopStakes")
            
            ###########################################################
            ## Wait before presenting release cue: ISI (unpredictable, 0.1-0.4 sec.)
            
            # Draw oyster again:
            instr_image.setImage('Instructions/Oyster_verysmall.png')
            instr_image.draw()

            # Fixation cross on screen:
            center_fix.setText('+')
            center_fix.draw()
            win.flip()
            
            # Wait specified time:
            waitforbutton(waitTime = 0, maxWait = ISI, waitTillEnd = True) # allow for abort
                
        ###########################################################
        ## V) Present release cues:
        
        # Initialize variables to be stored:
        response = 0 # NoGo as default
        respSide = 'NA' # gets only defined for Go
        respLate = 'NA' # gets only defined for Go
        RT = 'NA' # gets only defined for Go

        ACC = 'NA' # gets only defined for releaseCue trials
        validity = 'NA' # gets only defined for releaseCue trials
        outcome = 'NA' # gets only defined for releaseCue trials

        if isReleaseCue:
            
            if isCatch:
                if isPrint:
                    print('Catch trial')
                
                ######################################
                ## 1) Draw instructions for catch trial:
                
                if rewLeft == 1:
                    instr_image.setImage('Instructions/MGNGSearch_FreeView_CatchQuestionA.png') # reward left
                else:
                    instr_image.setImage('Instructions/MGNGSearch_FreeView_CatchQuestionB.png') # reward right	            	
                instr_image.draw()
                                                                
                win.flip() # flip for all images
                
                if useEyeTracker and (blockType == 'Test'):
                    getEYELINK().sendMessage("StartCatch")
                    
                ######################################
                ## 2) Wait for response:
                
                (response, respSide, RT) = waitforbutton(waitTime = 0, maxWait = catchPresent, waitTillEnd = False)                
                            
                if useEyeTracker and (blockType == 'Test'):
                    getEYELINK().sendMessage("StopCatch")
                ######################################
                ## 3) Code recognition accuracy:
                
                countCatch += 1 # increment number of recognition trials overall
                
                if response == 1: # if response made
                    
                    if respSide == reqSide: # if response on correct side
                        ACC = 1
                        countCatchCorrect += 1 # increment number of correct recognition trials
                    
                    else:
                        ACC = 0
                        
                    print('Recognition trials: {} out of {} correct'.format(countCatchCorrect,countCatch))
                    # Draw continue message:
                    function_display_instructions('Instructions/MGNGSearch_FreeView_CatchContinue.png',waitTime = 0.1, maxWait = waitAfterCatch-0.1, waitTillEnd = True)
                    
                else: # if NoGo: draw respond faster message
                    function_display_instructions('Instructions/MGNGSearch_FreeView_CatchTooSlow.png',waitTime = 0.1, maxWait = waitAfterCatch-0.1, waitTillEnd = True)
        
            ####################################################################
            ####################################################################
            ####################################################################
            
            else: # if standard trial
                
#                print('Oyster closing...')

                ###########################################################
                # 1) Present oyster image:
                
                # Select oyster image:
                if reqSide == 1:
                    center_image.setImage('TaskStimuli/ReleaseCues/oyster_left.png')
                else:
                    center_image.setImage('TaskStimuli/ReleaseCues/oyster_right.png')
                center_image.draw()
                
                # Draw can image:
                can_image.setImage('TaskStimuli/ReleaseCues/can_up.png')
                can_image.draw()
                
                # Present both:
                win.flip()
    
                # Start message to eye-tracker:
                if useEyeTracker and (blockType == 'Test'):
                    getEYELINK().sendMessage("StartReleaseCue")
    
                ###########################################################
                ## 2) Wait for button press:
                
                (response, respSide, RT) = waitforbutton(waitTime = 0, maxWait = releasePresent, waitTillEnd = False)
                                                    
                if response == 1:
                    
                    # Make can_image turn :
                    center_image.draw()
                    if respSide == 1:
                        can_image.setImage('TaskStimuli/ReleaseCues/can_left.png')
                    else:
                        can_image.setImage('TaskStimuli/ReleaseCues/can_right.png')
                    can_image.draw()
                    win.flip()
    
                    # Wait till end of response deadline:
                    endWait = round(releasePresent-RT,3)
                    if isPrint:
                        print('Wait additional {} seconds'.format(endWait))
                    core.wait(endWait)
                        
                # Stop message to eye-tracker:
                if useEyeTracker and (blockType == 'Test'):
                    getEYELINK().sendMessage("StopReleaseCue")
                    
                # Flip window so stimuli disappear:
                win.flip()
                    
                ###########################################################
                ## 3) ISI Keep recording late responses:
                
                # Fixation cross on screen:
                center_fix.setText('+')
                center_fix.draw()
                win.flip()
                
                # Keep recording late responses:
                (lateResponse, lateRespSide, lateRT) = waitforbutton(waitTime = 0, maxWait = fbDelay, waitTillEnd = True)
                                
                # If late responses: overwrite initial responses:
                if response == 1: # if response in time: never encode late responses
                    respLate = 0
                    
                elif response == 0 and lateResponse == 1: # if before no response, but now late response:
                    if isPrint:
                        print('Recorded late response')
                    respLate = 1
#                    response = lateResponse # overwrite initial response or not? If overwrite: need to check ACC beforehand!!!!
                    respSide = lateRespSide # overwrite initial response or not?
                    RT = releasePresent + lateRT # RT plus given time for release
                                            
                ###########################################################
                ## 4) Check accuracy (after ISI):
                                            
                if (response == 0) and (reqAction == 0): # if correct NoGo
                    ACC = 1
                
                elif (response == 1 and reqAction == 1) and (respSide == reqSide): # if Go and correct side
                    ACC = 1
                
                else: # otherwise wrong
                    ACC = 0
                    
                if isPrint:
                    print(u'ACC is {}'.format(str(ACC)))

                ###########################################################
                ## 5) Outcome:
                
                if isOutcome:
        
                    ######################################
                    ## a) Determine relevant validity given response:
                    
                    if response == 1:
                        validity = goValidity
                    else:
                        validity = nogoValidity

                    # Add potential reward for valid trials:
                    if validity == 1:
                        countValidPoints += rewMag # for valid feedback: correct response would give reward
                    else:
                    	countValidPoints -= punMag #  for invalid feedback: correct response would give punishment
                    
                    if isPrint:
                        print(u'Validity is {}'. format(str(validity)))
        
                    ######################################
                    ## b) Determine feedback:
                    
                    if (ACC == 1 and validity == 1) or (ACC == 0 and validity == 0):
                        out_image.setImage(rewStakes)
                        outcome = rewMag # for storage in output file
                        countPoints += rewMag
                        
                    elif (ACC == 1 and validity == 0) or (ACC == 0 and validity == 1):
                        out_image.setImage(punStakes)
                        outcome = punMag * -1 # for storage in output file
                        countPoints -= punMag
                        
                    else: 
                        print(u'Error: Cannot determine outcome given ACC = {}, validity = {}.'.format(str(ACC),str(validity)))
                        abort()
                                                
                    if isPrint:
                        print(u'Outcome is {}'. format(str(outcome)))
                    print(u'Points (this block) are {}'. format(str(countPoints)))
        
                    ######################################
                    ## c) Present outcome:
                    
                    if useEyeTracker and (blockType == 'Test'):
                        getEYELINK().sendMessage("StartOutcome")
                    
                    out_image.draw()
                    win.flip()
                    core.wait(fbPresent) # Present outcome for 700 ms
                    win.flip()

                    if useEyeTracker and (blockType == 'Test'):
                        getEYELINK().sendMessage("StopOutcome")

        ###########################################################
        ## XI) Write to eye-tracker at the end of trial:
        if useEyeTracker and (blockType == 'Test'):
            getEYELINK().sendMessage("EndTrial")
            
            getEYELINK().sendMessage("cue")
            getEYELINK().sendMessage("{}".format(cue))
            getEYELINK().sendMessage("reqAction")
            getEYELINK().sendMessage("{}".format(reqAction))
            getEYELINK().sendMessage("reqSide")
            getEYELINK().sendMessage("{}".format(reqSide))
            getEYELINK().sendMessage("rewLeft")
            getEYELINK().sendMessage("{}".format(rewLeft))            
            getEYELINK().sendMessage("angle")
            getEYELINK().sendMessage("{}".format(angle))            
            getEYELINK().sendMessage("rewMag")
            getEYELINK().sendMessage("{}".format(rewMag))
            getEYELINK().sendMessage("punMag")
            getEYELINK().sendMessage("{}".format(punMag))

            getEYELINK().sendMessage("response")
            getEYELINK().sendMessage("{}".format(response))
            getEYELINK().sendMessage("respSide")
            getEYELINK().sendMessage("{}".format(respSide))
            getEYELINK().sendMessage("ACC")
            getEYELINK().sendMessage("{}".format(ACC))
            getEYELINK().sendMessage("RT")
            getEYELINK().sendMessage("{}".format(RT))
            
            getEYELINK().sendMessage("outcome")
            getEYELINK().sendMessage("{}".format(outcome))
        # end of action selection part

        ###########################################################
        ## XII) write results (but only of actual trials, not practice trials)
        
        if blockType == 'Test':
            
            # Write trial-by-trial results to csv file:
            file_trials = open('BehavData/MGNGSearch_FreeView_Study_{:0>2d}_{:0>2d}.csv'.format(subject,outputAddon),'a') # open output file
            file_trials.write("{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}\n".format(
            subject,age,gender,hand,eye,sessionNr,blockNr,trialNr,
            cue,reqAction,reqSide,
            rewLeft,angle,rewMag,punMag,validity,
            response,respSide,respLate,ACC,RT,outcome,
            isCatch))
            
            file_trials.close()
            
        # End of trials
        
    print(u'Final point count of block: {}'.format(str(countPoints)))
    print(u'End of block')
    print(u'####################################################################')

    ###########################################################
    ## X) Calculate global performance statistics
    
    if useEyeTracker and ((blockType == 'Test') or testGazeContingent): # if eyetracker used and either Test recording or gaze contingency needed: start eye tracker
#        pylink.endRealTimeMode();    
        EYELINK.stopRecording();

    if countCatch > 0: # during test trials
        ratioCatch = float(countCatchCorrect)/countCatch #
    else: # during practice trials if not catch trials (don't divide by zero)
        ratioCatch = 0

    # Wait 1 second before advancing to next function
    center_fix.setText('+')
    center_fix.draw()
    win.flip()
    core.wait(1)
    
    return(countTrial,countPoints,countValidPoints,ratioCatch)

###########################################################################
# Run block including instructions and practice stimuli
###########################################################################

def function_block(blockType = None, blockNr = None, testGazeContingent = False):
    
    # Load stimuli with trialhandler
    if blockType == 'Pract':
        stimulusList = data.importConditions("TrialHandler/stimuluslist_pract_block{}.csv".format(blockNr)) # Load test stimuli   
    
    elif blockType == 'Test':
        stimulusList = data.importConditions("TrialHandler/stimuluslist_test_stimNum_{}_sub_{:0>3d}_block{}.csv".format(nTrials,subject, blockNr)) # Load test stimuli   
    
    else:
        outputtext = (u'Error: Invalid settings for coming block.')
        print(outputtext)
        core.quit()        
    
    # Prepare trials:    
    trials = data.TrialHandler(stimulusList, nReps = 1, method = "sequential")
    
    # Do task:
    (countTrial,countPoints,countValidPoints,ratioCatch) = function_run_main_trials(trials = trials, blockType = blockType, blockNr = blockNr, testGazeContingent = testGazeContingent)
    
    return(countTrial,countPoints,countValidPoints,ratioCatch)
    
###################################################################################
# Main function (allows for running the program if executed from the file itself)
###################################################################################

def function_main():
    
    allTrials = [] # empty list that will contain number of performed trials per block
    allPoints = [] # empty list that will contain points of each block
    allValid = [] # empty list that will contain number of performed trials per block
    allCatch = [] # empty list that will contain points of each block

    #--------------------------------------------------------------------------
    # Display instructions at beginning:
    
    if isFull == True and skipPractice == False:
        # General instructions:
        function_display_instructions('Instructions/MGNGSearch_FreeView_Welcome01.png')
        if subject % 2 == 1:
            function_display_instructions('Instructions/MGNGSearch_FreeView_Welcome02a.png')
            function_display_instructions('Instructions/MGNGSearch_FreeView_Welcome03a.png')
        else:
            function_display_instructions('Instructions/MGNGSearch_FreeView_Welcome02b.png')
            function_display_instructions('Instructions/MGNGSearch_FreeView_Welcome03b.png')
        function_block(blockType = 'Pract', blockNr = 1, testGazeContingent = False)

        # Intro demands:
        if subject % 2 == 1:
            function_display_instructions('Instructions/MGNGSearch_FreeView_Demand01a.png')
        else:
            function_display_instructions('Instructions/MGNGSearch_FreeView_Demand01b.png')
        function_display_instructions('Instructions/MGNGSearch_FreeView_Demand02.png')
        function_display_instructions('Instructions/MGNGSearch_FreeView_Demand03.png')
        function_block(blockType = 'Pract', blockNr = 2, testGazeContingent = False)
        
        # Intro stakes:
        function_display_instructions('Instructions/MGNGSearch_FreeView_Stakes01.png')
        if subject % 2 == 1: 
            function_display_instructions('Instructions/MGNGSearch_FreeView_Stakes02a.png')
            function_display_instructions('Instructions/MGNGSearch_FreeView_Stakes03a.png')
            function_display_instructions('Instructions/MGNGSearch_FreeView_Stakes04.png')
            function_display_instructions('Instructions/MGNGSearch_FreeView_Stakes05a.png')
        else:
            function_display_instructions('Instructions/MGNGSearch_FreeView_Stakes02b.png')
            function_display_instructions('Instructions/MGNGSearch_FreeView_Stakes03b.png')
            function_display_instructions('Instructions/MGNGSearch_FreeView_Stakes04.png')
            function_display_instructions('Instructions/MGNGSearch_FreeView_Stakes05b.png')

        function_block(blockType = 'Pract', blockNr = 3, testGazeContingent = False)
        
        # Intro catch trials:
        function_display_instructions('Instructions/MGNGSearch_FreeView_Catch01.png')
        function_display_instructions('Instructions/MGNGSearch_FreeView_Catch02.png')
        function_display_instructions('Instructions/MGNGSearch_FreeView_Catch03.png')
        function_display_instructions('Instructions/MGNGSearch_FreeView_Catch04.png')
        function_block(blockType = 'Pract', blockNr = 4, testGazeContingent = False)

        # Gaze contingency:
        if useGazeContingent:
            if subject % 2 == 1: 
                function_display_instructions('Instructions/MGNGSearch_FreeView_Gaze01a.png')
            else:
                function_display_instructions('Instructions/MGNGSearch_FreeView_Gaze01b.png')
            function_block(blockType = 'Pract', blockNr = 5, testGazeContingent = useGazeContingent)

    ###########################################################
    # Final instructions for test:
    function_display_instructions('Instructions/MGNGSearch_FreeView_Test01.png')
    function_display_instructions('Instructions/MGNGSearch_FreeView_Test02.png')

    # --------------------------------------------------------
    # Block 1:
    function_display_instructions('Instructions/MGNGSearch_FreeView_Block01Start.png')
    function_display_instructions('Instructions/MGNGSearch_FreeView_Remember.png')
    (countTrial,countPoints,countValidPoints,ratioCatch) = function_block(blockType = 'Test', blockNr = 1, testGazeContingent = useGazeContingent)
    allTrials.append(countTrial)
    allPoints.append(countPoints)
    allValid.append(countValidPoints)
    allCatch.append(ratioCatch)

    ###########################################################
    if isFull:
        function_display_instructions('Instructions/MGNGSearch_FreeView_Block01Stop.png') # first break only if full task

        # --------------------------------------------------------
        # Block 2:
        function_display_instructions('Instructions/MGNGSearch_FreeView_Block02Start.png')
        function_display_instructions('Instructions/MGNGSearch_FreeView_Remember.png')
        (countTrial,countPoints,countValidPoints,ratioCatch) = function_block(blockType = 'Test', blockNr = 2, testGazeContingent = useGazeContingent)
        allTrials.append(countTrial)
        allPoints.append(countPoints)
        allValid.append(countValidPoints)
        allCatch.append(ratioCatch)
        function_display_instructions('Instructions/MGNGSearch_FreeView_Block02Stop.png')

        # --------------------------------------------------------
        # Block 3:
        function_display_instructions('Instructions/MGNGSearch_FreeView_Block03Start.png')
        function_display_instructions('Instructions/MGNGSearch_FreeView_Remember.png')
        (countTrial,countPoints,countValidPoints,ratioCatch) = function_block(blockType = 'Test', blockNr = 3, testGazeContingent = useGazeContingent)
        allTrials.append(countTrial)
        allPoints.append(countPoints)
        allValid.append(countValidPoints)
        allCatch.append(ratioCatch)

#    ###########################################################
#    ## Compute points and overall catch accuracy:
    print('Points per block: {}'.format(allPoints))
    
    # a) Compute sum of all obtained points in all blocks:
    totalTrials = sum(allTrials) # sum of actually performed trials
    totalPoints = sum(allPoints) # sum of points
    totalValid = sum(allValid) # sum of all possible points
    totalCatch = numpy.mean(allCatch) # mean accuracy at recognition task
    
    if totalPoints < 20:
        totalPoints = 20
        print('Corrected output:')
    
    # b) Compute minimal and maximal points:
    print('totalPoints: {}'.format(totalPoints))
    print('totalCatch: {}'.format(round(totalCatch,2)))
    
    requiredPoints = 0
    possiblePoints = totalValid

    print('requiredPoints: {}'.format(str(requiredPoints)))
    print('possiblePoints: {}'.format(str(possiblePoints)))
    
    # c) Euros:
    euros = round(float(totalPoints - requiredPoints)/float(possiblePoints-requiredPoints)*maxPayoff,2) # euros gained
    if euros > 2: # set to maxPayoff if bigger than maxPayoff
        euros = maxPayoff
    
    ###########################################################
    ## print to screen and console:
    
    # a) First print to console: (for security in case participants advance task too fast)
    outputtext = ('You have gained {} points').format(str(totalPoints))
    print(outputtext)
    outputtext = ('and peformed correctly in {}% of number comparisons.').format(str(round(totalCatch*100,2)))
    print(outputtext)
    outputtext = ('So in total {:1.2f} euros extra'.format(euros))
    print(outputtext)
    
    # b) Then present on screen:    
    sentence_text.setPos((0,.2)) # first line of text
    sentence_text.setText(u'The task is over now. You have gained {} points.'.format(str(totalPoints)))
    sentence_text.draw()
#    sentence_text.setPos((0,.2)) # second line of text
#    sentence_text.setText(u'and performed correctly in {}% of number comparisons.'.format(str(round(totalCatch*100))))
#    sentence_text.draw()
    sentence_text.setPos((0,0)) # third line of text
    if totalPoints >= requiredPoints: # and totalCatch >= catchCutoff:
        sentence_text.setText(u'This gives you {:1.2f} euros extra!'.format(euros))
    else:
        sentence_text.setText(u'Unfortunately, that is not enough for an extra reward.')
    sentence_text.draw()
    sentence_text.setPos((0,-.2)) # fourth line of text
    sentence_text.setText(u'Please contact the experimenter now.')
    sentence_text.draw()
    win.flip() # finally flip

    # c) Press button twice again to abort:
    waitforbutton(waitTime = 4,maxWait = 120) # prevent participants from advancing task prematurely 
    win.flip() # clean screen 
    # function_display_instructions('Instructions/MGNGSearch_FreeView_End.png')
    
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################
###################################################################################################################################################################################


###################################################################################################################################################################################
## Clean console

clear = lambda: os.system('cls')
clear()

###################################################################################################################################################################################
## Create global variables: task structure

nSes = 3 # 3 sessions: needed to check whether input file exists
nCues = 4 # 4 cues per block 

## Get initial participant info:
(subject, age, gender, hand, eye, isFull, useButtonBox, useEyeTracker, useGazeContingent, skipPractice, nTrials) = function_gui_info()
# Create output file addon
outputAddon = random.randrange(0,100)
    
###########################################################################
## Create global variables: presentation settings

# Timings:
countdownPresent    = 1.0 # 0.8

ITIintercept        = 0.0
actionCuePresent    = 0.7 # 0.7 # 1.0
stakesPresent       = 1.5 # 1.3 # 1.5 # 2.0
ISIintercept        = 0.0
checkInterval       = 0.020 # check gaze every 20 ms
releasePresent      = 0.6 # 0.5
fbDelay             = 0.7 # so 0.6 + 0.7 = 1.3 sec in total waiting for response
fbPresent           = 1.0 # feedback presentation time

catchPresent        = 5 # 5 seconds
waitAfterCatch      = 1.0 # 1 seconds

# Image stimuli:
elevation           = 0.0 # elevation of cue above potential outcomes

# Width of left_image and right_image is 0.20, so 1920/2*0.20 = 192 pixels.
# To be non-overlapping, they must be at least 192/2 = 96 pixels displaced to the left/right.
# So eccentricity must be at least 96/(1920/2) = 0.1 to be non-overlapping.
eccentricity        = 0.30 # 0.3 # 0.15 # between 0 and 1

# eccentricity of 0.25 means displacement by 1920/2*0.25 = 240 pixels --> tolFix must be smaller than than
# if too close to 240 pixels: slight random displacement from middle position renders one stakes visible immediately, will saccade to it
# screen size is 1920, 1080, represented in norm units as -1,1, so 2 long
# # displacement by 0.25 means displacement by 1920/2*0.25 = 240 pixels

# Width of stakes images: 0.15, in pixels: 1920/2*0.15 = 144.0

# Tolerance in pixels:
tolFix      		= 150 # 150 

# Text stimuli:
textHeight          = 0.15 # 0.2 0.3
isPrint             = False

# Accuracy cut-off for points:
pointsCutoff        = 0.5 # 60% accuracy
probValid           = 23/28 # 75% of trials outcome is valid
maxPayoff           = 2.25 # pay 2€ maximum

###########################################################################
## Colors:

# RGB to Python conversion and back:
# a = numpy.array([-1.0,0.2,-0.6])
# reverse: (a+1)/2*255.0

# Background color:

bgColor =  numpy.array([180,180,180])/255.0*2-1 # grey # 180 all

# Stimulus colors:
color1 = 'orange' # 200, 100, 7 
color2 = 'blue' # 104, 104, 255
color1Fuzzy = 'TaskStimuli/StakesCues/orange_fuzzy.png'
color2Fuzzy = 'TaskStimuli/StakesCues/blue_fuzzy.png'

if subject % 2 == 1: # odd subject numbers: reward orange, punishment blue
    rewColor = color1
    rewFuzzy = color1Fuzzy
    punColor = color2
    punFuzzy = color2Fuzzy

    backgroundRewLeft = 'Instructions/OysterOrangeLeft.png'
    backgroundRewRight = 'Instructions/OysterOrangeRight.png'
    
else: # even subject numbers: reward blue, punishment orange
    rewColor = color2
    rewFuzzy = color2Fuzzy
    punColor = color1
    punFuzzy = color1Fuzzy

    backgroundRewLeft = 'Instructions/OysterOrangeRight.png'
    backgroundRewRight = 'Instructions/OysterOrangeLeft.png'

blackColor = [-1.0,-1.0,-1.0]
greyColor = [0,0,0]

###########################################################################
## Create Window:

# Mind: color in PsychoPy is between -1 and 1, so use rgb/255*2-1 or (python+1)/2*255 for conversion
# use 'norm' so positions are relative to screen size (100%)
# old background was .30,.30,.30, so 230,230,230
# new background is .01,.01,.01, so 180,180,180 (in line with calibration)

# win = visual.Window(size = (1920, 1080), color=bgColor, winType='pygame', units = 'norm') # fullscr=True
win = visual.Window( monitor="testMonitor", size = (1920, 1080), color=bgColor, units = 'norm', fullscr=True)
print('After setting window')

# Retrieve screen size for comparison with gaze position measured with eye-tracker
scrx = win.size[0]
scry = win.size[1] 
imag_scale = 0.8

print('Screen size is {}, {}'.format(scrx,scry)) # Screen size is 1920, 1080; middle is 960, 540
win.setMouseVisible(False) # render mouse invisible globally

###########################################################################
## ROIs:

#leftROI     = [scrx/2 - (scrx/2)*eccentricity,scry/2- (scry/2)*elevation] # horizontally middle (half of screen) minus half of screen times eccentricity; vertically just middle of screen
#rightROI    = [scrx/2 + (scrx/2)*eccentricity,scry/2 - (scry/2)*elevation] # horizontally middle (half of screen) plus half of screen times eccentricity; vertically just middle of screen

###########################################################################
## Create other task objects:

## Images:
instr_image = visual.ImageStim(win=win,image='Instructions/MGNGSearch_FreeView_Welcome01.png') # size: norm goes from -1 to 1, so 2, but pixelated
# Image sizes: X/Y = 0.625 ratio: (0.30, 0.48) (0.20, 0.32) or (0.15, 0.24) or (0.10, 0.16)
center_image= visual.ImageStim(win=win,image='TaskStimuli/ActionCues/A1.png', pos = (0,elevation), size = (0.30, 0.48), units = 'norm') # central cue on every trial
left_image  = visual.ImageStim(win=win,image='TaskStimuli/ActionCues/A1.png', pos = (-1*eccentricity,elevation), size = (0.15, 0.24), units = 'norm') # left stakes
right_image = visual.ImageStim(win=win,image='TaskStimuli/ActionCues/A1.png', pos = (1*eccentricity,elevation), size = (0.15, 0.24), units = 'norm') # right stakes
can_image   = visual.ImageStim(win=win,image='TaskStimuli/ActionCues/A1.png', pos = (0,0.18), size = (0.10, 0.16), units = 'norm') # tin can during response--rather small
out_image   = visual.ImageStim(win=win,image='TaskStimuli/ActionCues/A1.png', pos = (0,elevation), size = (0.20, 0.32), units = 'norm') # central cue on every trial

## Text objects:
# wrapWidth = 0.80
center_fix  = visual.TextStim(win=win,text="", pos = (0,0), color=blackColor, wrapWidth = 0.8, alignText = 'center', height = textHeight) # Test out color: .80? .30 textHeight = 0.20
sentence_text = visual.TextStim(win=win,text="Starting up task...", pos = (0.0,0), color=blackColor, wrapWidth = 0.8, alignText = 'center', height = textHeight*0.6) # longer text: textHeight = 0.20 * 0.3

# Start and print start message:
sentence_text.draw()
win.flip()
core.wait(0.5)

# Define trial output file:
file_trials = open('BehavData/MGNGSearch_FreeView_Study_{:0>2d}_{:0>2d}.csv'.format(subject,outputAddon),'w') # write headers of file: 
file_trials.write("{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}\n".format(
'subject','age','gender','hand','eye','sessionNr','blockNr','trialNr',
'cue','reqAction','reqSide',
'rewLeft','angle','rewMag','punMag','validity',
'response','respSide','respLate','ACC','RT','outcome',
'isCatch'))
file_trials.close()

###########################################################################
## Initialize button box:

if useButtonBox: 
    print('Initialize button box')
    bb = serial.Serial("COM3", 115200) # initialize buttonbox # used to be COM3
    # bb = serial.Serial() # initialize buttonbox
    core.wait(3) # wait 3 seconds to get BITSI m message
    bb.read(bb.inWaiting()) # read/ delete any bytes from bb
    
###########################################################################
## Connect to Eyelink:

if useEyeTracker:
    sentence_text.setText(u'Starting eye-tracker...')
    sentence_text.draw()
    win.flip()
    edfFileName = "s_{:0>2d}_{:0>2d}.EDF".format(subject,outputAddon) # write headers of file: 
    EYELINK = EyeLink('100.1.1.1') #EyeLinkListener();
    EYELINK.openDataFile(edfFileName)    
    # Actually start recording:
    error = EYELINK.startRecording(1,1,0,0) # 
    # Initialize pl:
    eyelink = pylink.EyeLink()
    
    # Stop recording again for practice trials:
    EYELINK.stopRecording()
        
###########################################################################
## Run experiment
###########################################################################

function_main()

###########################################################################
## End experiment
###########################################################################
## Stop recording and save idf file, disconnect from iViewX
if useEyeTracker:
    sentence_text.setText(u'Close eye-tracker file...')
    sentence_text.draw()
    win.flip() # print closing message
    EYELINK.setOfflineMode();
    msecDelay(500); # in ms; 500 enough
    #Close the file and transfer it to Display PC
    EYELINK.closeDataFile()
    EYELINK.receiveDataFile(edfFileName, edfFileName)
    print('Finished closing eye tracker file')
###########################################################################
## Quit
core.quit()
# END of program