# Shiny app for Quanthammer
# author: ian hussey (ian.hussey@ugent.be)
# license: GPLv2+

# dependencies ----

library(shiny)
library(tidyverse)
#library(pbapply)
#library(viridisLite)


# functions ----

roll_dice <- function(shots_or_attacks,
                      balistic_or_weapon_skill,
                      reroll_to_hit_all,
                      reroll_to_hit_ones,
                      hit_modifier,
                      strength,
                      toughness,
                      reroll_to_wound_all,
                      reroll_to_wound_ones,
                      wound_modifier,
                      save,
                      save_modifier,
                      reroll_saves_all,
                      reroll_saves_ones,
                      ignore_wound_criterion,
                      damage,
                      wounds_per_target) {
  
  # strength vs toughness
  wound_criterion  <-
    if (strength/toughness <= 0.5) {
      6
    } else if (strength/toughness >= 2) {
      2
    } else if (strength/toughness < 1) {
      5
    } else if (strength/toughness > 1) {
      3
    } else {
      4
    }
  
  # find N hits
  if (reroll_to_hit_all == FALSE & reroll_to_hit_ones == FALSE){
    # roll to hit
    hit_rolls           <- sample(1:6, shots_or_attacks, replace = TRUE)
    # add modifiers
    hit_rolls_modified  <- hit_rolls + hit_modifier
    # convert rolls less than 1 to 1, as rolls have a hard lower bound regardless of modifiers
    hit_rolls_modified[hit_rolls_modified < 1] = 1
    # find number of hits
    hits <- sum(hit_rolls_modified >= balistic_or_weapon_skill)
  } else if (reroll_to_hit_all) {
    # roll to hit
    hit_rolls           <- sample(1:6, shots_or_attacks, replace = TRUE)
    # fine natural hits
    hits_natural        <- hit_rolls[hit_rolls >= balistic_or_weapon_skill]
    n_hits_natural      <- length(hits_natural)
    # make a number of rerolls equal to the number of shots - natural hits
    hit_rerolls         <- sample(1:6, shots_or_attacks - n_hits_natural, replace = TRUE)
    # combine the raw natural rolls and rerolls
    hit_rolls_combined  <- append(hits_natural, hit_rerolls)
    # add modifiers. this is done after rerolls as 40k 8th edition specifies modifier are applied after rerolls.
    hit_rolls_modified  <- hit_rolls_combined + hit_modifier
    # convert rolls less than 1 to 1, as rolls have a hard lower bound regardless of modifiers
    hit_rolls_modified[hit_rolls_modified < 1] = 1
    # find number of hits
    hits <- sum(hit_rolls_modified >= balistic_or_weapon_skill)
  } else if (reroll_to_hit_ones) {
    # roll to hit
    hit_rolls           <- sample(1:6, shots_or_attacks, replace = TRUE)
    # fine natural hits
    hits_natural        <- hit_rolls[hit_rolls >= balistic_or_weapon_skill]
    # make a number of rerolls equal to the number of shots - natural hits
    hit_rerolls         <- sample(1:6, sum(hit_rolls == 1), replace = TRUE)
    # combine the raw natural rolls and rerolls
    hit_rolls_combined  <- append(hits_natural, hit_rerolls)
    # add modifiers. this is done after rerolls as 40k 8th edition specifies modifier are applied after rerolls.
    hit_rolls_modified  <- hit_rolls_combined + hit_modifier
    # convert rolls less than 1 to 1, as rolls have a hard lower bound regardless of modifiers
    hit_rolls_modified[hit_rolls_modified < 1] = 1
    # find number of hits
    hits <- sum(hit_rolls_modified >= balistic_or_weapon_skill)
  }
  
  # find N wounds
  if (reroll_to_wound_all == FALSE & reroll_to_wound_ones == FALSE){
    # roll to wound
    wound_rolls           <- sample(1:6, hits, replace = TRUE)
    # add modifiers
    wound_rolls_modified  <- wound_rolls + wound_modifier
    # convert rolls less than 1 to 1, as rolls have a hard lower bound regardless of modifiers
    wound_rolls_modified[wound_rolls_modified < 1] = 1
    # find number of wounds
    wounds <- sum(wound_rolls_modified >= wound_criterion)
  } else if (reroll_to_wound_all) {
    # roll to wound
    wound_rolls           <- sample(1:6, hits, replace = TRUE)
    # fine natural wounds
    wounds_natural        <- wound_rolls[wound_rolls >= wound_criterion]
    n_wounds_natural      <- length(wounds_natural)
    # make a number of rerolls equal to the number of shots - natural wounds
    wound_rerolls         <- sample(1:6, shots_or_attacks - n_wounds_natural, replace = TRUE)
    # combine the raw natural rolls and rerolls
    wound_rolls_combined  <- append(wounds_natural, wound_rerolls)
    # add modifiers. this is done after rerolls as 40k 8th edition specifies modifier are applied after rerolls.
    wound_rolls_modified  <- wound_rolls_combined + wound_modifier
    # convert rolls less than 1 to 1, as rolls have a hard lower bound regardless of modifiers
    wound_rolls_modified[wound_rolls_modified < 1] = 1
    # find number of wounds
    wounds <- sum(wound_rolls_modified >= wound_criterion)
  } else if (reroll_to_wound_ones) {
    # roll to wound
    wound_rolls           <- sample(1:6, hits, replace = TRUE)
    # fine natural wounds
    wounds_natural        <- wound_rolls[wound_rolls >= wound_criterion]
    # make a number of rerolls equal to the number of shots - natural wounds
    wound_rerolls         <- sample(1:6, sum(wound_rolls == 1), replace = TRUE)
    # combine the raw natural rolls and rerolls
    wound_rolls_combined  <- append(wounds_natural, wound_rerolls)
    # add modifiers. this is done after rerolls as 40k 8th edition specifies modifier are applied after rerolls.
    wound_rolls_modified  <- wound_rolls_combined + wound_modifier
    # convert rolls less than 1 to 1, as rolls have a hard lower bound regardless of modifiers
    wound_rolls_modified[wound_rolls_modified < 1] = 1
    # find number of wounds
    wounds <- sum(wound_rolls_modified >= wound_criterion)
  }
  
  # find N armour saves
  if (reroll_saves_all == FALSE & reroll_saves_ones == FALSE){
    # roll to wound
    armour_save_rolls           <- sample(1:6, wounds, replace = TRUE)
    # add modifiers
    armour_save_rolls_modified  <- armour_save_rolls + save_modifier
    # convert rolls less than 1 to 1, as rolls have a hard lower bound regardless of modifiers
    armour_save_rolls_modified[armour_save_rolls_modified < 1] = 1
    # find number of failed armour saves
    failed_armour_saves <- sum(armour_save_rolls_modified < save)
  } else if (reroll_saves_all) {
    # roll to wound
    armour_save_rolls           <- sample(1:6, wounds, replace = TRUE)
    # fine natural failed armour saves
    failed_armour_saves_natural <- armour_save_rolls[armour_save_rolls < save]
    n_armour_saves_natural      <- length(failed_armour_saves_natural)
    # make a number of rerolls equal to the number of shots - natural armour_save
    armour_save_rerolls         <- sample(1:6, shots_or_attacks - n_armour_saves_natural, replace = TRUE)
    # combine the raw natural rolls and rerolls
    armour_save_rolls_combined  <- append(armour_save_rolls, armour_save_rerolls)
    # add modifiers. this is done after rerolls as 40k 8th edition specifies modifier are applied after rerolls.
    armour_save_rolls_modified  <- armour_save_rolls_combined + save_modifier
    # convert rolls less than 1 to 1, as rolls have a hard lower bound regardless of modifiers
    armour_save_rolls_modified[armour_save_rolls_modified < 1] = 1
    # find number of failed armour saves
    failed_armour_saves <- sum(armour_save_rolls_modified < save)
  } else if (reroll_saves_ones) {
    # roll to wound
    armour_save_rolls           <- sample(1:6, wounds, replace = TRUE)
    # fine natural wounds
    failed_armour_saves_natural <- armour_save_rolls[armour_save_rolls < save]
    # make a number of rerolls equal to the number of shots - natural wounds
    armour_save_rerolls         <- sample(1:6, sum(armour_save_rolls == 1), replace = TRUE)
    # combine the raw natural rolls and rerolls
    armour_save_rolls_combined  <- append(armour_save_rolls, armour_save_rerolls)
    # add modifiers. this is done after rerolls as 40k 8th edition specifies modifier are applied after rerolls.
    armour_save_rolls_modified  <- armour_save_rolls_combined + save_modifier
    # convert rolls less than 1 to 1, as rolls have a hard lower bound regardless of modifiers
    armour_save_rolls_modified[armour_save_rolls_modified < 1] = 1
    # find number of wounds
    failed_armour_saves <- sum(armour_save_rolls_modified < save)
  }
  
  # roll ignore wounds (feel no pain, disgustingly resiliant, etc)
  failed_armour_saves_not_ignored <- sum(sample(1:6, failed_armour_saves, replace = TRUE) < ignore_wound_criterion)
  
  damage_splash_discarded <- ifelse(damage > wounds_per_target, wounds_per_target, damage)
  
  n_dice_per_kill <- ceiling(wounds_per_target / damage_splash_discarded)
  
  n_killed <- floor(failed_armour_saves_not_ignored / n_dice_per_kill)
  
  n_non_fatal_wounds_on_last_minature <- failed_armour_saves_not_ignored - n_killed*n_dice_per_kill
  
  if (n_non_fatal_wounds_on_last_minature != 0) {
    result <- paste(n_killed, "models were killed outright, with one additional model suffering", n_non_fatal_wounds_on_last_minature, "wound(s)")
  } else {
    result <- paste(n_killed, "models were killed outright")
  }
  
  return(list(result                               = result,
              n_killed                             = n_killed,
              n_non_fatal_wounds_on_last_minature  = n_non_fatal_wounds_on_last_minature))
}


# ui ----

ui <- 
  
  navbarPage("Quanthammer",
             
             # tab 1 - overview ----
             tabPanel("Roll dice",  
                      
                      # Sidebar panel for inputs
                      sidebarPanel(
                        
                        # input
                        numericInput(inputId = "shots_or_attacks", 
                                     label = "Attacks (A or gun stat, eg 'Assault 2')", 
                                     value = 20, 
                                     min = 1, 
                                     max = NA),
                        
                        numericInput(inputId = "balistic_or_weapon_skill", 
                                     label = "Ballistic/Weapon skill (BS/WS)", 
                                     value = 3, 
                                     min = 2, 
                                     max = 6),
                        
                        numericInput(inputId = "strength", 
                                     label = "Strength (S)", 
                                     value = 4, 
                                     min = 1, 
                                     max = 30),
                        
                        numericInput(inputId = "toughness", 
                                     label = "Toughness (T)", 
                                     value = 4, 
                                     min = 1, 
                                     max = 30),
                        
                        numericInput(inputId = "save",
                                     label = "Save (Sv)", 
                                     value = 3, 
                                     min = 2, 
                                     max = 7),
                        
                        numericInput(inputId = "damage",
                                     label = "Damage (D)", 
                                     value = 1, 
                                     min = 1, 
                                     max = 10),
                        
                        numericInput(inputId = "wounds_per_target",
                                     label = "Wounds per target (W)", 
                                     value = 1, 
                                     min = 1, 
                                     max = 100),
                        
                        numericInput(inputId = "hit_modifier", 
                                     label = "Hit roll modifier", 
                                     value = 0, 
                                     min = 0, 
                                     max = 5),
                        
                        numericInput(inputId = "wound_modifier", 
                                     label = "Wound roll modifier", 
                                     value = 0, 
                                     min = 0, 
                                     max = 5),
                        
                        numericInput(inputId = "save_modifier",
                                     label = "Save modifier", 
                                     value = 0, 
                                     min = 0, 
                                     max = 7),
                        
                        # reroll_to_hit_all
                        # reroll_to_hit_ones
                        selectInput(inputId = "reroll_to_hit", 
                                    label = "Reroll to hit",
                                    choices = c("None"  = "none",
                                                "Reroll ones"  = "ones",
                                                "Reroll all" = "all")),
                        
                        # reroll_to_wound_all
                        # reroll_to_wound_ones
                        selectInput(inputId = "reroll_to_wound", 
                                    label = "Reroll to wount",
                                    choices = c("None"  = "none",
                                                "Reroll ones"  = "ones",
                                                "Reroll all" = "all")),
                        
                        # reroll_saves_all
                        # reroll_saves_ones
                        selectInput(inputId = "reroll_saves", 
                                    label = "Reroll to save",
                                    choices = c("None"  = "none",
                                                "Reroll ones"  = "ones",
                                                "Reroll all" = "all")),
                        
                        numericInput(inputId = "ignore_wound_criterion",
                                     label = "Ignore wound special rule (e.g., feel no pain)", 
                                     value = 7, 
                                     min = 2, 
                                     max = 7)
                        
                      ),
                      
                      # Main panel for displaying outputs
                      mainPanel(
                        
                        actionButton("roll", "Roll dice"),
                        
                        h3(textOutput("results_text"))
                        
                      )
             )
  )


# server ----

server <- function(input, output) {
  
  event_roll_dice <- eventReactive(input$roll, {
    
    results_text <- roll_dice(shots_or_attacks         = input$shots_or_attacks,
                              balistic_or_weapon_skill = input$balistic_or_weapon_skill,
                              reroll_to_hit_all        = ifelse(input$reroll_to_hit == "all", TRUE, FALSE),
                              reroll_to_hit_ones       = ifelse(input$reroll_to_hit == "ones", TRUE, FALSE),
                              hit_modifier             = input$hit_modifier,
                              strength                 = input$strength,
                              toughness                = input$toughness,
                              reroll_to_wound_all      = ifelse(input$reroll_to_wound == "all", TRUE, FALSE),
                              reroll_to_wound_ones     = ifelse(input$reroll_to_wound == "ones", TRUE, FALSE),
                              wound_modifier           = input$wound_modifier,
                              save                     = input$save,
                              save_modifier            = input$save_modifier,
                              reroll_saves_all         = ifelse(input$reroll_saves == "all", TRUE, FALSE),
                              reroll_saves_ones        = ifelse(input$reroll_saves == "ones", TRUE, FALSE),
                              ignore_wound_criterion   = input$ignore_wound_criterion,
                              damage                   = input$damage,
                              wounds_per_target        = input$wounds_per_target)
    
    results_text$result
    
  })
  
  output$results_text <- renderText({
    
    Sys.sleep(1.5)
    event_roll_dice()
    
  })
  
}


# build app ----

shinyApp(ui, server)


