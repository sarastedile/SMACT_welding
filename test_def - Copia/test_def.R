
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(DT)
library(readr)
library(devtools)
library(DoE.base)
library(FrF2)
library(bslib)
library(ggplot2)
library(car)
library(carData)
library(MASS)
library(tidyverse)

#Function buttons --------------------------------------------------------------

styleButtonBlue<- function(){
  "white-space: normal;
                        text-align:center;
                        color: white; 
                        background-color:slateblue;
                        border-color: white;
                        border-width:3px;
                        height:35px;
                        width:180px;
                        font-size: 13px;"
}

#Images can be found in folder "www" -------------------------------------------

img2 <- tags$figure(
  align = "center",
  tags$img(
    src = "pic3.jpg",
    width = 130,
    alt = "Lap joint." ),
  tags$figcaption("Image of example lap joint."))

imgdc <- tags$figure(
  align = "center",
  tags$img(
    src = "pic1.jpg",
    width = 200,
    alt = "Insufficient oxide removal." ),
  tags$figcaption("Image of insufficient oxide removal: AC balance set too high."))

img1 <- tags$figure(
  align = "center",
  tags$img(
    src = "pic2.jpg",
    width = 140,
    alt = "Butt joint." ),
  tags$figcaption("Image of example butt joint."))

img3 <- tags$figure(
  align = "center",
  tags$img(
    src = "pic4.jpg",
    width = 300,
    alt = "Corner joint." ),
  tags$figcaption("Image of example corner joints."))

img4 <- tags$figure(
  align = "center",
  tags$img(
    src = "pic8.jpg",
    width = 250,
    alt = "edge joint." ),
  tags$figcaption("Image of example edge joints."))

img5 <- tags$figure(
  align = "center",
  tags$img(
    src = "pic7.jpg",
    width = 130,
    alt = "T joint." ),
  tags$figcaption("Image of example T-joint."))

img6 <- tags$figure(
  align = "center",
  tags$img(
    src = "pic9.webp",
    width = 900,
    alt = "welding_pos" ),
  tags$figcaption("Image of example welding positions."))

# Load data --------------------------------------------------------------------

n_factors <- names(read_delim("Factors.csv", delim = ";", quote = "'",
                              escape_double = FALSE, locale = locale(decimal_mark = ","),
                              na = "NA", trim_ws = TRUE))

text_jconfig <- column(8,
                       list(br(),
                            h5("The first influence on welding parameters is certainly the type of joint that must be subjected to welding. 
                            In aid of a classification as clear as possible, five basic kinds are defined."),
                            br(),
                            fluidRow( 
                              column(5, img1),
                              column(4,
                                     h4("butt joint"),h6("faying edges lie in the same plane and are joined 'end-to-end';"))),
                            
                            fluidRow( 
                              column(5, img3),
                              column(4,
                                     h4("corner joint"), h6("faying edges lie perpendicularly one to the other, such as to form a right angle. The weldment can be drawn on inside or outer corner;"),
                              )), #end column4
                            
                            fluidRow( 
                              column(5, br(), br(),img4),
                              column(4,
                                     h4("edge joint"), h6("faying surfaces are parallel, such as the ends are placed 'side by side' and welded on the full piece thickness. It is reserved for workpieces that have flanging rims or that are in need for a weld made to attach to adjacent pieces;"),
                              )), #end column4 
                            
                            fluidRow( 
                              column(5, img2),
                              column(4,
                                     h4("lap joint"), h6("faying surfaces overlap and are welded at edges; usually employed in case of thin metal sheets or of works of different thickness. It is characterized by one or both-sided weld (the latter to provide an extra reinforcement);"),
                              )), #end column4
                            
                            fluidRow( 
                              column(5, img5),
                              column(4,
                                     h4("tee joint"), h6("workpieces are perpendicular, to 'build a sort of letter T' and faying edges are welded."),
                              )), #end column4
                            
                            h6("Moreover, the biggest distinction is between fillet and groove joints.
                        The former represent a weld beside the two workpieces, and not between, thus 
                        it does not restore the physical continuity between the parts. Their resisting 
                        cross section and stress state must be carefully designed, since they constitute 
                        inherently a discontinuity. Usually they are more economical and simple or no edge 
                        preparation is required; anyway, their use is reserved to all those applications with 
                        low stresses involved; to enhance their adaptivity, double-fillet welds can be executed, 
                        with same or different welds size, in order to suit to severe loads. Another issue in fillet 
                        welding is forming the right weld size: it could be mitre, concave or convex, in relation to 
                        the desired throat thickness (ideally the root length, the distance between vertex and basis 
                        of the largest 45° right triangle that can be inscribed in the weld cross section). Compared 
                        to groove joints, they need more amperage for the same thickness, as they determine more heat 
                        dispersion, leading to possible fusion defects.",
                               style="text-align: justify;"),br(),
                            h6("Groove joints are chosen when full-thickness strength is desired, hence suitable for thicker 
                        materials; it is characterized by a weld which attests edge preparation in order to achieve 
                        better adhesion, complete penetration or a smooth appearance. Edges can be beveled with a 
                        specific angle (bevel, square, J, U, V-shaped), partially (bead weld) or completely throughout 
                        material thickness, single or both sided on a workpiece.",
                               style="text-align: justify;"),br(),
                            h6("Differently from fillet welds, groove ones show weldment area also belonging to the metal base 
                        workpiece: for this reason the base material must have a good weldability.",
                               style="text-align: justify;"),br(),
                            h6("Although a minimum amount of additional weld metal is preferable, groove joints are often 
                        characterized by a (number of) layering needed to cover the rut and by the eventual presence of 
                        a specific-tolerance space in-between workpieces, also ensured by special designed consumable 
                        inserts while root passing.", style="text-align: justify;")))

text_jposit <- column(8,
                      list(br(),
                           h5("In TIG welding all welding positions are actually feasible. However it is important to remember the 
                        work with a robotic welder arm, therefore with the addition of limits such as the size of the arm and its 
                        mobility with respect to the workpiece, in terms of possible singularities. Positions are here intended 
                        according to norm DIN EN ISO 6947; even if the concept is similar to other norms like ASME and ASW, they have a 
                        different naming system."), br(),
                           h4("PA or 1F, 1G"),
                           h6("the flat position, in which the welder has the workpiece joint below the torch, on a horizontal plane; it is 
                    common for groove weld, quite wanted and not requiring many skills to complete;", style="text-align: justify;"),
                           
                           h4("PB, PC or 2F, 2G"),
                           h6("it is similar to the flat position, but it has anyway to fight against force of gravity. To do so, generally the molten 
                    metal puddle should be 'less heaten up and less fluid', so that it would not fall out of the groove joint, expecially 
                    whenever there is no bottom shelf. Moreover, it is advisable to set the torch tip work angle such to favour the superior 
                    edge of the joint;", style="text-align: justify;"),
                           
                           h4("PD, PE or 4F, 4G"),
                           h6("the commonly described overhead position for butt welds: since robotic arm would hold the 
                       torch below the workpiece joint and angle itself to reach it, some precautions in terms of parameter selection must 
                       be undertaken.", style="text-align: justify;"),
                           h6("First of all, the molten metal puddle should be kept small and more solid, like in vertical positions, to avoid 
                    excessive dropping and sagging. To pursue it, a manipulation of the weld puddle is needed to adhere to the groove rims 
                       better.", style="text-align: justify;"),
                           
                           h4("PF or 3F, 3G up"),
                           h6("it is a welding in vertical position, upwards, generally reserved for robust butt or fillet joint on workpieces 
                    that are handled with difficulty: starting from lower parts of workpiece, robotic welder has to deal with the 
                    gravity force and a higher risk of burn through. For this reason, to keep low the heat input and besides to keep the molten 
                    metal puddle adherent to the joint, the amperage and the eventual filler rod feed rate are decreased; as travelling 
                    up, a quick move across the middle of the puddle is suggested (zigzag pattern, upsidedown T or repeated triangles motion); 
                    also parameters for thinner workpieces can be used; ", style="text-align: justify;"),
                           
                           h4("PG or 3F, 3G down"),
                           h6("it is a welding in vertical position, downwards. It's a quite good position in productivity terms: since the weld 
                     puddle is granted by the fused metal dropping from above, a rapid feed rate is requested and less penetration is 
                     determined, making this joint configuration suitable for thin workpieces or for root passes. ", 
                              style="text-align: justify;")))

text_feedr <- column(8,
                     list(h6("Travel speed is the advancement rate of the welding torch, therefore it strictly affects the deposition speed of 
                     construction of the welding seam. Furthermore, in the case of automated welding machines, it should be remembered that, 
                     although the torch and the filler wire feed system are structurally connected, the latter demonstrates independent rate, 
                     extrusion or rewinding characteristics."),br(),
                          h6("It is a factor sensible to base material kind (in terms of thermal conductivity) and arc length or current amperage."),br(),
                          h6("Whenever it results too high, weld penetration is incomplete, there is less weld reinforcement, more tendency to 
                     undercut and porosity, weld pool appears narrow; contrariwise there is an unacceptable protrusion of root with more 
                     undercut, excessively wide weld and slag inclusions, tendency to burn-through."),br(),
                          h6("Therefore is generally comprised in range 200-400 [mm/min]; thin thickness workpieces, low conductivity materials, 
                     high amperage require more speed.")),
                     style="text-align: justify;")

text_gasr <- column(8,
                    list(h6("In a GTAW application, shielding gas is of extreme relevance."),br(),
                         h6("First of all the gas exits a diffuser, a nozzle cup, some kind of tool that is posed on the welding torch.
                         This induce to a variation in its flow: from a desiderable laminar one, the change in velocity and density once 
                         it contacts atmospheric gas surrounding it may lead to a turbulent flow."),br(),
                         h6("The latter determines contamination phenomena: if atmospheric oxide, nitrogen, hydrogen are pulled into the core 
                         of arc column, some particles can vapour or precipitate, causing spatter in the molten pool, unwanted inclusions 
                         which then solidify affecting weld beam; the electrode can get dirty and lose its fusion capacity, as well as having 
                         detrimental effects on the penetration and shape of the weld seam."),br(),
                         h6("Another harmful feature is the lost of the shielding power, causing an easy access of air pockets into arc column, 
                         thus spatter, again contamination and porosity in weld bead."),br(),
                         h6("For such reasons the design of the diffuser to channel the gas properly and the gas flow rate must be set carefully, 
                         in accordance to welding torch consumable and tools design, atmospheric conditions like the wind presence, joint 
                         configuration, base material cleaning and desired results."),br(),
                         h6("The gas flow rate can be finely adjusted with a fluxometer located as close as possible to the operation site. 
                         The rate is comprised in the approximate range 4-20 [l/min]: too low values induce to poor coverage, bad 
                         quality of weld seam and easy arc column disturbance, while too high ones to turbulence and to a disproportionate 
                         gas consumption compared to the weld deposition."),br(),
                         h6("If the objective of gas flow is to provide protective coverage, it is necessary to verify that the gas is emitted 
                         directly from the nozzle and perfectly covers the arc and the weld pool; moreover, in the case of complex shape 
                         workpieces, it is advisable to check whether the gas flow undergoes deflections due to the rebound of the gas 
                         against the pieces.")),
                    style="text-align: justify;")

text_ppfl <- column(8,
                    list(h6("Pre-flow of shielding gas accounts for the time needed from the gas to cover weld areas before starting the arc, 
                    so to have a hose-free flushing and an additional grooves cleaning. It is a parameter with little appreciable values, 
                    since usually it is set on range 0.1-0.5 [s]."),br(),
                         h6("Post-flow of shielding gas is instead the time during which gas is still flowing after the arc has extinguished, 
                         to safely let weld pool solidify and tungsten electrode cool down without oxidation (therefore lengthening electrode 
                         tip lifetime). The factor can be set around value of 8 [s] for most works; it is very influential however for 
                            stainless steel especially.")),
                    style="text-align: justify;")


text_udslope <- column(8,
                       list(h6("Before introducing the concepts of up and down slope time as regards the current characteristic, it must be 
                    explained that there is a method of using the torch generally defined as '4-Touch' (4T), which provides for a gradual 
                    climb in the welding amperage through a first stage, so-called initial current, and a subsequent ramp up to the operative 
                    welding current. The same scheme can be used for the descending phase, recognizing the down slope time ramp and the crater 
                    current."),br(),
                            h6("With the first switch of manual torch, pre-flow gas and then initial current are reached. With the second, starts the 
                    upslope until maximum amperage; it is customarily not executed for thick workpieces and some aluminum clear anodized 
                    welds. If the trigger is on again, weld is stopped and amperage decreases with downslope until crater current; however 
                    arc is quit only once trigger is let off and hence post-flow of shielding gas occurs."),br(),
                            h6("Indicating the time required for a gentle transition between the ignition and effective currents (or vice versa), they 
                    help the heat introduced into the workpiece to be attenuated, so that in general less base material overheating 
                    and burn through take place."),br(),
                            h6("They are generally set to values of about 1-2 [s]. ")),
                       style="text-align: justify;")

text_iccurr <- column(8,
                      list(h6("Initial current allows for a stable arc ignition with a high level current and a sort of gentle pre-heat of 
                       workpiece metal, as well as a correct positioning of the filler rod material."),br(),
                           h6("Crater current is exploited to avoid crater like cracks, defect that is visible especially at the end of butt 
                            joint welds, shrink holes and porosity; it helps reducing gradually the current value, weld pool and heat 
                            concentration."),br(),
                           h6("They are commonly set as percentage of the welding current, sometimes also indicated in welding reference table."),br(),
                           h6("It is advisable not to confuse the initial/crater currents with the base one of the pulsed mode, as they are independent entities.")),
                      style="text-align: justify;")
text_pbcurr <- column(8,
                      list(h6("Current without doubts is one of the parameters which most influence welding process. It expresses the direct 
                       control on arc heating and metal melting. Predictably, the use of high currents leads to greater heating of the pieces 
                       and of the tungsten electrode itself, thus consuming it faster (it should be remembered that the consumption of the electrode 
                       is more linked to marginal and slower wear phenomena), and resulting in a larger weld pool. Moreover, also the risk of 
                       undercut in weld seam and tungsten particles contamination are enhanced. On the other side, too low values of current 
                       may determine incomplete penetration and a deficient fusion of metals, with arc root wandering and instability."),br(),
                           h6("The use of greater amperage increases the electrical energy in the weld site faster, such that metal deposition rate and 
                      travel speed of machine are consequently augmented."),
                           h6("There are two major constraints on the value of the current: the limit that can be supplied by the power source without 
                      this incurring overheating and the maximum value that can be tolerated by the tungsten electrode, according to its chemical 
                      composition and diameter size."),
                           h6("Nevertheless, the optimal current range is also influenced by the use of alternating current in terms of balance between 
                      negative and positive half-wave current. Larger positive half-cycle means reduced current-carrying capacity of electrode."),br(),
                           h6("Whenever pulsed modality in DC current is selected, parameter setting changes substantially. This technique offers the 
                      advantages of increasing the depth-to-width ratio in weld seam and of minimizing heat affected zone. 
                      The background current, of lower value respect to the peak/pulse main welding one, grants the conservation of the arc; 
                      moreover, it permits the cooling and control of a smaller weld pool. 
                      Usually it is not available in reference charts; for this reason it is a fundamental intrinsic parameter that has to be 
                      investigated in terms of value. It can be also expressed in form of percentage of the high pulse amperage. Ordinary values 
                      can be 5-10 [A], less than 15% of peak current."),br(),
                           h6("Whenever it has a too high value, it is responsible for arc instability, its increase of length, heat affected zone and 
                      delocation. On the contrary, if too low value, for an incomplete mixing of base material and filler one molten metals."),br(),
                           h6("Pulse peak and base current help in building 'stacked dimes' aspect; in manual welding, currents can then be synchronized 
                      with the rate of wire feed, such that during the high current state, the filler material is added into molten pool.")),
                      style="text-align: justify;")

freq_intro <- column(8,
                     h5("To use AC polarity or a pulsed modality in DC determines an oscillation in welding current that is characterized by a frequency. 
              This lets a reduction in heat generation, a better welding speed and an aesthetical look similar to continuous TIG."),
                     style="text-align: justify;")

text_DCfreq <- column(8,
                      list(br(),
                           h6("Frequency is the indication of how many cycles are performed in one second of time. Higher frequency 
                           (50-500 or >15 [Hz]) stands for reduction in continuous heat, suitable for thin thickness or low conductivity 
                           material workpiece."),
                           h6("Lowest one (0.1-5 [Hz]) ends in weld seam where two visually clear periods (high/low current) of weld pools 
                           are present, where contour is wider and quality can be poor. Suitable to build 'stacked dimes' aspect."),
                           h6("Low frequency (<15 [Hz]) are used to prevent dripping in welding penetration and to the hanging of vertical 
                           fillet bead.")), style="text-align: justify;")

text_ACfreq <- column(8,
                      list(br(),
                           h6("Frequency in AC polarity is similar by theoretic point of view to DC one. Anyway, it is conventionally 
                           augmented to make arc column more stable and narrower, so that there is more directional control to outcome 
                           precise weld.",style="text-align: justify;"),
                           
                           h6(" > 400 [Hz] arc cone is tight, more stable, ideal for fillet or tight inside corner welds requiring penetration;"),
                           h6(" 250-150 [Hz] for thin base workpiece, to avoid overheating;"),
                           h6(" 120-80 [Hz] good arc control, faster travel speeds; suitable for thick base workpiece requiring more heat input;"),
                           h6(" 90-80 [Hz] for outside corner joints, resulting in a wider weld bead.")))

text_DCon <- column(8,
                    list(br(),
                         h6("Pulse width or duty cycle is the ratio defining the time spent at peak current on each pulse, expressed as 
                         percentage. In steel a common value is considered to be about 40%."),
                         h6("Whenever it is too high, greater melting and more heating of tungsten electrode occur;
                         in contrast whenever too low, also low is the heat or melting contribution.", 
                            style="text-align: justify;")))
text_ACon <- column(8,
                    list(br(),
                         h6("Pulse width/duty cycle in AC polarity assumes a different meaning: it is indeed the AC balance ratio defining 
                         the time spent at positive peak current (EP) on each pulse, expressed as percentage. Usually is set around values 
                         of 40-45%."),br(),
                         h6("To increase the cleaning power, that is to better break/remove all oxide shallow layer which would hinder metal 
                         fusion, it is better to set to 60-70%;"),
                         h6("if instead it is at low values, like 25%, the risk of 'peppering' defect is enhanced.")),
                    style="text-align: justify;")

text_elect <- column(8,
                     list(br(),
                          h6("Next to electrode composition, that must be previously decided, electrode diameter influences welding parameters and yield.
                        It is normally comprised in range of values 1-4.8 [mm] and should be properly sized for intended application."),
                          h6("Diameters have a strong relation with the maximum amount of current they can deliver. Fixing an amperage of reference, 
                         larger diameter means longer electrode duration but harder arc starting; in contrast, fixing a diameter: if excessive currents 
                         are used, overheating, erosion and melting of the electrode, contamination of weld pool can be the risks, while if lower ones, 
                         low temperature, unstable arc and electrode corrosion can be the results."),
                          h6("Some documents are available providing a totally general indication of current ranges for tungsten according to the diameter 
                         of electrode, in both DC and AC and with argon shielding gas. Routine rules are then the following:"),
                          h6("DCEP requires larger electrode diameter, since electrode is heaten up;"),
                          h6("in AC welding electrode can handle an amperage value in-between the capacity of an electrode on DCEN (maximum) and 
                         DCEP (minimum)."), br(),
                          h6("Also the shape of the electrode is a conditioning aspect; in particular its tip defines how energy is focused on 
                         workpiece, thus the concentration and stability of the arc. 
                        The electrode undergoes a grinding operation in order to shape the tip, paying attention that the electrode is 
                        abraded perpendicularly to the axis of the fine grain size grinding wheel; if direct current is used, the tip will 
                        typically be sharpened, constituting an angle of about 30°, to assure penetration depth and narrow weld seam. 
                        Therefore best grinding height is between 1.5 and 3 times the diameter length of the electrode."),
                          h6("With practical examples it is observable that the ground angle affects arc pressure: the greater the grinding angle 
                        carried out on the tip, the smaller the width and pressure of the arc (it tends more towards a column shape than a 
                        truncated cone one) and therefore greater penetration and smaller width of the weld bead. However, for a high 
                        depth-to-width ratio on thin plates, it is better use a small vertex angle."),
                          h6("Welders then prefer to flatten the end, so as to achieve greater arc stability. More pointed tips (10-25°) 
                        are used for lower amperages."), br(),
                          h6("Whenever and AC welding process is executed or pure and zirconiated tungsten electrodes are present, the tip should 
                            be hemispherical.")),
                     style="text-align: justify;")

text_filler <- column(8,
                      list(br(),
                           h6("The addition of a filler material, rod in manual welding and wire in automated one, lead to the reduction of heat 
                           available for fusion at weld pool.
                           
                            The filler metals are commonly available in different diameter sizes and are made with several alloys, to best suit 
                            application. Some reference table expressing the relations are usually provided.
                            
                            In general, filler rod diameter is lower than the thickness of base workpiece; when using low current values, too 
                            large diameter may cause weld pool cooling and an irregular weld seam."), br(),
                           h6("For what concerns the feed system, it determines filler wire feed rate independently from torch travel speed, 
                            although the latter is a referred factor.
                            
                            Whenever it is selected too slow, not sufficient filler material is put into weld pool, causing penetration issues, 
                            narrow weld seam with enhanced risk of poor quality, structural integrity and defects like cracks or discoloration. 
                            Moreover, wire can burn back, form some balls that are then melted on contact tip.
                            
                            Whenever wire feed rate is too high: wire consumes quickly, there are burn-through risk due to higher penetration 
                            and spatter risk, a wider weld is obtained, acoustic feedback is present.")),
                      style="text-align: justify;")






# ------------------------------------------------------------------------------ 
# ------------------------------------------------------------------------------ 
# ------------------------------------------------------------------------------                   
# Define UI --------------------------------------------------------------------

ui <- navbarPage("DoE",
                 theme=bs_theme(version = 4, bootswatch = "default"),
                 
                 # Start tabpanel of parameters -------------------------------------------------                
                 
                 tabPanel("Parameters selection",
                          
                          # Application title, opens main panel for visualization
                          h2("Determine basic parameters collection for a welding operation", style = "color:#BD559C"),#titlePanel("Determine basic parameters collection for a welding operation."),
                          br(),
                          
                          mainPanel(
                            h4("The search of the parameters with which to start the design of experiment
      begins with the upload of available reference charts. These, taken from
      literature, like papers or welder handbooks, are often incomplete, constituting
      therefore an obstacle.", style = "font-family: 'colibri'; font-si16pt; color:black; text-align:justify"),
                            br(),
                            h4("In the next part of the web page, a general and coarse procedure is 
       proposed to guide the definition of parameters, based on the choice of 
       the basic features of the welding to be carried out.", 
                               style = "font-family: 'colibri'; font-si16pt; color:black; text-align:justify"),
                            hr(),hr(),
                            
                            fluidRow(
                              
                              # Eterogeneity of base material section
                              column(6,
                                     radioButtons("eter", "Base materials of the workpieces to be welded result:",
                                                  c("-" ="-",
                                                    "the same" = "sam",
                                                    "different" = "diff"))),
                              column(6,
                                     conditionalPanel(
                                       condition = "input.eter == 'diff'",
                                       selectInput(inputId = "eter_mat",
                                                   label = "Choose the workpiece material combination:",
                                                   choices = c("None" = 1, "Mild Steel + Copper" = 2, "Mild Steel + Stainless Steel" = 3),
                                                   selected = 1),
                                       uiOutput("out_etermat"))
                                     
                              )),
                            conditionalPanel(
                              condition = "input.eter == 'sam'",
                              
                              # Gap section
                              fluidPage(
                                theme=bs_theme(version = 4, bootswatch = "default"),
                                hr(),
                                column(6,
                                       radioButtons("isgap", "Workpieces to be welded are separated by a gap?",
                                                    c("-" ="-",
                                                      "Yes" = "yes",
                                                      "No" = "no"))),
                                column(6,
                                       conditionalPanel(
                                         condition = "input.isgap == 'yes'",
                                         uiOutput("out_gap"),
                                         selectInput(inputId = "gap",
                                                     label = "Choose the base material of first workpiece:",
                                                     choices = c("None" = 1, "Stainless steel" = 2, "Mild steel" = 3),
                                                     selected = 1)
                                         )
                                )),
                              hr(),
                              fluidPage(
                                theme=bs_theme(version = 4, bootswatch = "default"),
                                conditionalPanel(
                                  condition = "input.eter == 'sam' && input.isgap == 'no'",
                                  
                                  selectInput(inputId = "dataset1",
                                              label = "Choose the base material of first workpiece:",
                                              choices = c("None" = 1, "Aluminum"= 2,
                                                          "Stainless steel" = 3, "Mild steel" = 4,
                                                          "Low alloy steel" = 5, "Killed steel"= 6),
                                              selected = 1),
                                  #Aluminum section --------------------------------------------------------------          
                                  conditionalPanel(
                                    condition = "input.dataset1 == '2' ",
                                    fluidPage(
                                      theme=bs_theme(version = 4, bootswatch = "default"),
                                      h4(" Aluminum welding requires AC modality, thus a welding polarity characterized 
                   by a balanced exchange of both ions and electrons between electrode tip and surface
                   of workpiece material.", 
                                         style = "font-family: 'colibri'; font-si15pt; color:black; text-align:justify"),
                                      br(),
                                      checkboxGroupInput(inputId='showAL', "Variables to show:",
                                                         choices = 'Filler material'),
                                      
                                      h4("First of all, it is essential to determine the desired characteristics of the weld bead, in order to set 
                the parameters accordingly.", 
                                         style = "font-family: 'colibri'; font-si15pt; color:black; text-align:justify"),
                                      br(),
                                      column(6,
                                             radioButtons("waveform", "Select the shape of the amperage wave:",
                                                          c("Square/standard" = "r",
                                                            "Sinusoidal/soft" = "s",
                                                            "Hard" = "p",
                                                            "Triangular" = "t"))),
                                      column(6,
                                             textOutput("out_waveform"))),
                                    
                                    
                                    fluidPage(
                                      theme=bs_theme(version = 4, bootswatch = "default"),
                                      
                                      # Input: Selector for choosing fixed main parameters
                                      column(6,
                                             h4("Then, the:",
                                                style = "font-family: 'colibri'; font-si15pt; color:black; text-align:justify"),
                                             selectInput("a_tj", "Type of joint:",
                                                         list("All", "butt", "lap", "corner", "fillet"))
                                      ),
                                      column(6,
                                             selectInput("a_oj", "Orientation of joint:",
                                                         list("All", "PA", "PE", "PF", "PG"))
                                      ),
                                      column(6,
                                             selectInput("a_thick1", "Thickness of plates [mm]:",
                                                         choices = "")
                                      ),
                                      column(6,
                                             selectInput("a_elect_d", "Electrode diameter [mm]:",
                                                         choices = "")
                                      ),
                                      br(),
                                      br(),
                                      tabsetPanel(
                                        id = 'al_table_panel_out',
                                        tabPanel("Filler material",
                                                 conditionalPanel(
                                                   condition = "input.showAL.indexOf('Filler material') !== -1",
                                                   dataTableOutput("table_al"))),
                                        tabPanel("No filler material",
                                                 conditionalPanel(
                                                   condition = "input.showAL.indexOf('Filler material') !== 1" ,
                                                   dataTableOutput("table_nofmal")))
                                      )
                                    ) # end fluidPage
                                  ),    # end cond panel about Aluminum
                                  
                                  # Other materials section ------------------------------------------------------    
                                  conditionalPanel(
                                    condition = "input.dataset1 != 2 && input.dataset1 != 1",
                                    checkboxGroupInput(inputId='show', "Variables to show:",
                                                       choices = c('Filler material','Pulsed modality'),
                                                       selected = c(F,F)),
                                    br(),
                                    h4("The result of the search filters is as follows. If some values show no
              wording or the wording 'NA', it means that no indications are available at
              the moment, and therefore they must be analysed further.", 
                                       style = "font-family: 'colibri'; font-si15pt; color:black; text-align:justify"),
                                    
                                    hr(),
                                    
                                    fluidPage(
                                      theme=bs_theme(version = 4, bootswatch = "default"),
                                      # Input: Selector for choosing fixed main parameters
                                      column(3,
                                             selectInput("tj", "Typology of joint:",
                                                         list("All", "butt", "lap", "corner", "fillet"))
                                      ),
                                      column(3,
                                             selectInput("oj", "Orientation of joint:",
                                                         list("All", "PA", "PE", "PF", "PG"))
                                      ),
                                      column(3,
                                             selectInput("thick1", "Thickness of plates [mm]:",
                                                         choices = "")
                                      ),
                                      column(3,
                                             selectInput("elect_d", "Electrode diameter [mm]:",
                                                         choices = "")
                                      )
                                      
                                    ), #close fluidPage
                                    
                                    br(),br(),
                                    fluidPage(
                                      theme=bs_theme(version = 4, bootswatch = "default"),
                                      conditionalPanel(
                                        condition = "input.show.indexOf('Pulsed modality') != 1 && input.show.indexOf('Filler material') != 1",
                                        tabPanel("No filler rod and pulse OFF", dataTableOutput("table_nofmpm"))),
                                      br(), br(), br(),
                                      tabsetPanel(
                                        id = 'table_panel_out',
                                        tabPanel("Filler material",
                                                 conditionalPanel(
                                                   condition = "input.show.indexOf('Pulsed modality') != 1 && input.show.indexOf('Filler material') != -1" ,
                                                   dataTableOutput("table_fm"))),
                                        tabPanel("Pulse ON",
                                                 conditionalPanel(
                                                   condition = "input.show.indexOf('Pulsed modality') != -1 && input.show.indexOf('Filler material') != 1" ,
                                                   dataTableOutput("table_pm"))),
                                        tabPanel("Filler material and pulse ON",
                                                 conditionalPanel(
                                                   condition = "input.show.indexOf('Pulsed modality') != -1 && input.show.indexOf('Filler material') != -1" ,
                                                   dataTableOutput("table_fmpm")))#dataTableOutput("table"))) #
                                      )
                                    )
                                    
                                    
                                  ) # end cond panel about other materials
                                ) # end same and no gap condition
                              ) # end first fluid page
                            ) # end cond panel same
                          ) # end main panel
                          
                 ), # end tab panel
                 # Tab panel of FP --------------------------------------------------------------
                 tabPanel("Missing parameters",
                          h2("Help for not defined value of parameters", style = "color:#BD559C"),
                          br(),
                          tabsetPanel(id = "firsttabset",
                                      type = "tabs",
                                      tabPanel(n_factors[1], text_jconfig),
                                      tabPanel(n_factors[2], text_jposit, img6),
                                      tabPanel(n_factors[4], text_elect),
                                      tabPanel("Filler material wire", text_filler),
                                      tabPanel(n_factors[6], text_feedr),
                                      tabPanel(n_factors[7], text_gasr),
                                      tabPanel(list(n_factors[9], "and", n_factors[20]), text_ppfl),
                                      tabPanel(list(n_factors[10], "and", n_factors[19]), text_udslope),
                                      tabPanel(list(n_factors[11], "and", n_factors[18]), text_iccurr),
                                      tabPanel(list(n_factors[12], "and", n_factors[13]), text_pbcurr),
                                      tabPanel("Duty cycle [%]",
                                               tabsetPanel(id = "secondtabset",
                                                           type = "tabs",
                                                           tabPanel(n_factors[14], uiOutput("text1")),
                                                           tabPanel(n_factors[15], uiOutput("text2"), imgdc)
                                               )),
                                      tabPanel("Frequency [Hz]",
                                               tabsetPanel(id = "thirdtabset",
                                                           type = "tabs",
                                                           freq_intro,
                                                           tabPanel(n_factors[16], uiOutput("text3")),
                                                           tabPanel(n_factors[17], uiOutput("text4"))
                                               ))
                          )
                          
                 ), #end tabpanel
                 
                 
                 # Tab panel of FP --------------------------------------------------------------
                 tabPanel("Factorial plan",
                          h2("Factorial plan: building design matrix", style = "color:#BD559C"),
                          column(8,
                                 h5("In this page an example of design of the tests to be performed 
            is delineated. With the aid of a R-property function of name FrF2,
            either a full factorial plan (increasing resolution tab) or a regular 
            fractional factorial plan with 2-levels and minimum aberration in a 
            fixed number of runs is provided.", style="text-align: justify;"),
                                 br(),h6("The number of runs corresponds to the smallest design of the 
            specified resolution that accomodates selected number of factors. 
            If the smallest possible design is a full factorial or not catalogued, 
            the function stops with an error.", style="text-align: justify;"),
                                 h6("In the box on the left you can therefore specify, in order, 
            the number of factors to consider for the design; the number 
            of replications (if 1, absent); the resolution to obtain; 
            whether to randomize the lines of the design.", 
                                    style="text-align: justify;"),
                                 h6("In the box on the right instead, a summary of all the 
               characteristics useful for the analysis is provided: first of all, 
               the number of runs to be performed; the factors settings according 
               to their levels and their number; the number of fractionals and the 
               related generators; the design aliases; the design iself, which 
               will always be provided here in a non-randomized way. Finally, the 
               class to which the FP belongs: full or fractional.", 
                                    style="text-align: justify;"),
                                 br()),
                          
                          sidebarLayout(
                            sidebarPanel(
                              tagList(
                                numericInput("nfactors", "Number of factors", value = 3),
                                numericInput("rep", "Replications", value = 1),
                                numericInput("res", "Resolution", value = 3),
                                numericInput("ncenter", "Number of center points", value = 0),
                                selectInput("rand", "Randomize treatments", 
                                            choices = c("TRUE", "FALSE"), selected = F)
                              ),
                              br(),
                              h5("Download design matrix to add yield column"), 
                              downloadButton("download_b1", label = "Download"),
                              br()),
                            mainPanel(
                              theme=bs_theme(version = 4, bootswatch = "default"),
                              dataTableOutput("design_output"),
                              verbatimTextOutput("sum")
                            ))
                          
                          
                 ), #end tabpanel
                 
                 
                 
                 # Tab panel of ANOVA -----------------------------------------------------------
                 
                 tabPanel("Statystical analysis",
                          sidebarLayout(
                            
                            sidebarPanel(
                              column(width = 12, align="center",
                                     fileInput("post_pro", "Choose .csv file with design and yield information", accept = ".csv"),
                                     tags$hr(),
                                     textInput("model", "Select the relation formula for the model", value="Y~A*"),
                                     verbatimTextOutput("value"),
                                     br(),
                                     tags$hr(),tags$hr(),
                                     conditionalPanel(
                                       condition = "input.int_graph == 1",
                                       box(
                                         textInput("in1", "Specify x-factor", value=""),
                                         textInput("in2", "Specify trace factor", value="")
                                       ),
                                       tags$hr(),tags$hr()
                                     ),
                                     h5("Select plot type"),
                                     actionButton(inputId = "qqn_graph",  label = "Q-Q norm plot",         style = styleButtonBlue()),
                                     actionButton(inputId = "int_graph",   label = "Interaction plot",     style = styleButtonBlue()),
                                     actionButton(inputId = "res_graph",  label = "Y vs residuals plot",   style = styleButtonBlue()),
                                     actionButton(inputId = "qqp_graph",  label = "Q-Q plot of residuals", style = styleButtonBlue()),
                                     tags$hr(),tags$hr(),
                                     actionButton(inputId = "bc_graph",   label = "Box-cox plot",          style = styleButtonBlue()),
                                     tags$hr(),tags$hr(),
                                     actionButton(inputId = "ANOVA",      label = "ANOVA analysis",        style = styleButtonBlue())
                              )
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Plots", uiOutput(outputId = "mplot")),                
                                tabPanel("Anova analysis", uiOutput("analysis")),
                                tabPanel("Box-Cox plot", plotOutput("boxcox"))
                              )
                            )# end mainpanel
                            
                          ) #end sidebarLayout     
                 ) #end tabpanel
) # end UI navbarpage


#-------------------------------------------------------------------------------
# Define Server ----------------------------------------------------------------
server <- function(input, output,session) {
  
  
  
  #Eterogenous material section
  output$out_etermat <- renderUI({
    switch(input$eter,
           sam = h5("Continue below", style = "color:blue"),
           diff = list(h5("Below is reported available reference chart for eterogeneous welding operations.", 
                          br(),"Note that pulsed modality and filler material options are here already considered, 
                            the latter indicating also the suggested filler rod material kind.",
                          style = "font-family: 'colibri'; font-si15pt; color:black; text-align:justify"), tableOutput("table_eter")))
  })
  
  Input_etermat <- reactive({
    switch(input$eter_mat,
           "Mild steel + Copper"= msc,
           "Mild steel + Stainless steel" = msss)
    
    if (input$eter_mat == 2) {
      data_etermat <- read_delim("Eter_steels.csv", delim = ";", quote = "'",
                                 escape_double = FALSE, locale = locale(decimal_mark = ","),
                                 na = "NA", trim_ws = TRUE)
      data_etermat <- data_etermat[1,]}
    if (input$eter_mat == 3) {
      data_etermat <- read_delim("Eter_steels.csv", delim = ";", quote = "'",
                                 escape_double = FALSE, locale = locale(decimal_mark = ","),
                                 na = "NA", trim_ws = TRUE)
      data_etermat <- data_etermat[2,]}
    
    if (input$eter_mat != 1) {head(data_etermat)}
  })
  
  output$table_eter <- renderTable({
    data_etermat <- Input_etermat()
    data_etermat
  })
  # ------------------------------------------------------------------------------
  # Gap section
  output$out_gap <- renderUI({
    switch(input$isgap,
           no = h5("Continue below",
                   style = "font-family: 'colibri'; font-si15pt; color:black; text-align:justify"),
           yes = list(h5("In presence of a gap between base workpieces to be welded, generally a filler rod is requested.
                         Moreover, the values of factors - like travel speed of cobot, amperage of welding operation and,
                         if pulsed modality is activated, frequency - are all decreased with increasing gap width.",
                         style = "font-family: 'colibri'; font-si15pt; color:black; text-align:justify"),
                      h5("Select a base material and see if desired reference chart to weld workpieces with gap is available",
                         style = "font-family: 'colibri'; font-si15pt; color:black; text-align:justify"),
                      tableOutput("table_gap")))
  })
  
  Input_gap <- reactive({
    switch(input$gap,
           "Stainless steel"= inox,
           "Mild steel" = ms)
    
    if (input$gap == 2) {
      data <- read_delim("gap_inox_steels.csv", delim = ";", quote = "'",
                         escape_double = FALSE, locale = locale(decimal_mark = ","),
                         na = "NA", trim_ws = TRUE)}
    if (input$gap == 3) {
      data <- read_delim("gap_mild_steels.csv", delim = ";", quote = "'",
                         escape_double = FALSE, locale = locale(decimal_mark = ","),
                         na = "NA", trim_ws = TRUE)}
    
    if (input$gap != 1) {head(data)}
  })
  
  output$table_gap <- renderTable({
    data_gap <- Input_gap()
    data_gap
  })
  
  # ------------------------------------------------------------------------------
  # Base material section
  datasetInput <- reactive({
    switch(input$dataset1,
           "Aluminum" = al,
           "Stainless steel"= inox,
           "Mild steel" = ms,
           "Low alloy steel" = lat,
           "Killed steel" = ks)
    
    if (input$dataset1 == 2) {
      data <- read_delim("Al.csv", delim = ";", quote = "'",
                         escape_double = FALSE, locale = locale(decimal_mark = ","),
                         na = "NA", trim_ws = TRUE)}
    if (input$dataset1 == 3) {
      data <- read_delim("Inox_steels.csv", delim = ";", quote = "'",
                         escape_double = FALSE, locale = locale(decimal_mark = ","),
                         na = "NA", trim_ws = TRUE)}
    if (input$dataset1 == 4) {
      data <- read_delim("Mild_steels.csv", delim = ";", quote = "'",
                         escape_double = FALSE, locale = locale(decimal_mark = ","),
                         na = "NA", trim_ws = TRUE)}
    if (input$dataset1 == 5) {
      data <- read_delim("Low_alloy_steels.csv", delim = ";", quote = "'",
                         escape_double = FALSE, locale = locale(decimal_mark = ","),
                         na = "NA", trim_ws = TRUE)}
    if (input$dataset1 == 6) {
      data <- read_delim("Killed_steels.csv", delim = ";", quote = "'",
                         escape_double = FALSE, locale = locale(decimal_mark = ","),
                         na = "NA", trim_ws = TRUE)}
    if (input$dataset1 != 1) {(data)}
  })
  
  # Main parameters section
  
  observeEvent(input$dataset1, {
    message("Table event observed")
    choices_thick <- datasetInput()$`Thickness of plates [mm]`
    choices_elec <- datasetInput()$`Electrode diameter [mm]`
    updateSelectInput(session, "thick1", choices = c("All", choices_thick ))
    updateSelectInput(session, "elect_d", choices = c("All", choices_elec ))
  })
  
  # ------------------------------------------------------------------------------
  output$table_fmpm <- renderDataTable(datatable({
    data<- datasetInput()
    if (input$tj != "All") {
      data <- data[data$`Type of joint` == input$tj,]
    }
    if (input$oj != "All") {
      data <- data[data$`Joint configuration` == input$oj,]
    }
    if (input$thick1 != "All") {
      data <- data[data$`Thickness of plates [mm]` == input$thick1,]
    }
    if (input$elect_d != "All") {
      data <- data[data$`Electrode diameter [mm]` == input$elect_d,]
    }
    br()
    data
  }, options = list(lengthMenu = c(5, 10, 30), pageLength = 5, na = "NA"))   )
  
  output$table_fm <- renderDataTable(datatable({
    dataa_fm <- datasetInput()
    dataa_fm <- dataa_fm[, c(1,2,3,4,5,6,7,8,9,10,11,12,16,17,18)] # no 13,14,15
    if (input$tj != "All") {
      dataa_fm <- dataa_fm[dataa_fm$`Type of joint` == input$tj,]
    }
    if (input$oj != "All") {
      dataa_fm <- dataa_fm[dataa_fm$`Joint configuration` == input$oj,]
    }
    if (input$thick1 != "All") {
      dataa_fm <- dataa_fm[dataa_fm$`Thickness of plates [mm]` == input$thick1,]
    }
    if (input$elect_d != "All") {
      dataa_fm <- dataa_fm[dataa_fm$`Electrode diameter [mm]` == input$elect_d,]
    }
    dataa_fm
  }, options = list(lengthMenu = c(5, 10, 30), pageLength = 5, na = "NA"))   )
  
  output$table_pm <- renderDataTable(datatable({
    dataa_pm <- datasetInput()
    dataa_pm <- dataa_pm[, c(1,2,3,4,6,7,9,10,11,12,13,14,15,16,17,18)] #no5and 8
    if (input$tj != "All") {
      dataa_pm <- dataa_pm[dataa_pm$`Type of joint` == input$tj,]
    }
    if (input$oj != "All") {
      dataa_pm <- dataa_pm[dataa_pm$`Joint configuration` == input$oj,]
    }
    if (input$thick1 != "All") {
      dataa_pm <- dataa_pm[dataa_pm$`Thickness of plates [mm]` == input$thick1,]
    }
    if (input$elect_d != "All") {
      dataa_pm <- dataa_pm[dataa_pm$`Electrode diameter [mm]` == input$elect_d,]
    }
    dataa_pm
  }, options = list(lengthMenu = c(5, 10, 30), pageLength = 5, na = "NA"))   )
  
  output$table_nofmpm <- renderDataTable(datatable({
    dataa_nofmpm <- datasetInput()
    dataa_nofmpm <- dataa_nofmpm[, c(1,2,3,4,6,7,9,10,11,12,16,17,18)]
    if (input$tj != "All") {
      dataa_nofmpm <- dataa_nofmpm[dataa_nofmpm$`Type of joint` == input$tj,]
    }
    if (input$oj != "All") {
      dataa_nofmpm <- dataa_nofmpm[dataa_nofmpm$`Joint configuration` == input$oj,]
    }
    if (input$thick1 != "All") {
      dataa_nofmpm <- dataa_nofmpm[dataa_nofmpm$`Thickness of plates [mm]` == input$thick1,]
    }
    if (input$elect_d != "All") {
      dataa_nofmpm <- dataa_nofmpm[dataa_nofmpm$`Electrode diameter [mm]` == input$elect_d,]
    }
    dataa_nofmpm
  }, options = list(lengthMenu = c(5, 10, 30), pageLength = 5, na = "NA"))   )
  
  # ------------------------------------------------------------------------------
  #Aluminum section
  al_wf1 <- "To obtain defined and large weld pool"
  al_wf2 <- "Effective for butt welding of thin worksheet, less defined margin are defined"
  al_wf3 <- "To obtain concentrated arc, for thin worksheet fillet weld or first layer groove weld"
  al_wf4 <- "To obtain sharp weld pool, for internal corner or edge joints"
  
  output$out_waveform <- renderText({
    switch(input$waveform,
           r = al_wf1,
           s = al_wf2,
           p = al_wf3,
           t = al_wf4
    )
  })
  observeEvent(input$dataset1, {
    message("Table event observed")
    choices_thick <- datasetInput()$`Thickness of plates [mm]`
    choices_elec <- datasetInput()$`Electrode diameter [mm]`
    updateSelectInput(session, "a_thick1", choices = c("All", choices_thick ))
    updateSelectInput(session, "a_elect_d", choices = c("All", choices_elec ))
  })
  
  output$table_al <- renderDataTable(datatable({
    data<- read_delim("Al.csv", delim = ";", quote = "'",
                      escape_double = FALSE, locale = locale(decimal_mark = ","),
                      na = "NA", trim_ws = TRUE)
    
    if (input$a_tj != "All") {
      data <- data[data$`Type of joint` == input$a_tj,]
    }
    if (input$a_oj != "All") {
      data <- data[data$`Joint configuration` == input$a_oj,]
    }
    if (input$a_thick1 != "All") {
      data <- data[data$`Thickness of plates [mm]` == input$a_thick1,]
    }
    if (input$a_elect_d != "All") {
      data <- data[data$`Electrode diameter [mm]` == input$a_elect_d,]
    }
    br()
    data}, options = list(lengthMenu = c(5, 10, 30), pageLength = 5, na = "NA"))
    
  )
  
  output$table_nofmal <- renderDataTable(datatable({
    dataa_nofmal <- datasetInput()
    dataa_nofmal <- dataa_nofmal[, c(1,2,3,4,6,7,9,10,11,12,13,14,15,16,17,18)]
    if (input$a_tj != "All") {
      dataa_nofmal <- dataa_nofmal[dataa_nofmal$`Type of joint` == input$a_tj,]
    }
    if (input$a_oj != "All") {
      dataa_nofmal <- dataa_nofmal[dataa_nofmal$`Joint configuration` == input$a_oj,]
    }
    if (input$a_thick1 != "All") {
      dataa_nofmal <- dataa_nofmal[dataa_nofmal$`Thickness of plates [mm]` == input$a_thick1,]
    }
    if (input$a_elect_d != "All") {
      dataa_nofmal <- dataa_nofmal[dataa_nofmal$`Electrode diameter [mm]` == input$a_elect_d,]
    }
    dataa_nofmal
  }, options = list(lengthMenu = c(5, 10, 30), pageLength = 5, na = "NA")) )
  
  
  # ------------------------------------------------------------------------------
  # Factorial design section -----------------------------------------------------
  frac_design <- function(nfactors = NULL, replications = 0, resolution = NULL, ncenter = 0, randomize = TRUE) {
    FrF2( nfactors = nfactors,
          replications = replications,
          resolution = resolution,
          ncenter = ncenter,
          randomize = randomize)  }
  
  design_table <- reactive({
    as.data.frame(
      frac_design(
        nfactors = input$nfactors,
        replications = input$rep,
        resolution = input$res,
        ncenter = input$ncenter,
        randomize = as.logical(input$rand)
      ))
  })
  
  output$sum <- renderPrint({
    summary(frac_design(
      nfactors = input$nfactors,
      replications = input$rep,
      resolution = input$res,
      ncenter = input$ncenter,
      randomize = as.logical("FALSE"))) 
  })
  
  output$design_output <- renderDataTable(
    design_table(),
    options = list(
      ordering = FALSE, searching = FALSE,
      lengthChange = FALSE, pageLength = 10, autoWidth = FALSE
    )
  )
  
  
  # Download partial table
  output$download_b1 <- downloadHandler(
    filename = function(){
      paste(design_table(), ".csv", sep = " ") 
    },
    content = function(file) {
      write.table(as.data.frame(design_table()), file,  #write.csv(design_table(), file),      
                  quote = TRUE, sep = ";",
                  na = "NA", dec = ".", row.names = FALSE,
                  col.names = TRUE)
    }
  )
  
  
  #-------------------------------------------------------------------------------
  # Help tabpanel ----------------------------------------------------------------
  observeEvent(c(input$firsttabset, input$secondtabset, input$thirdtabset), {
    
    if(input$firsttabset == "Duty cycle [%]" & input$secondtabset == n_factors[14]) {
      output$text1 <- renderUI({text_DCon})  }  
    
    if (input$firsttabset == "Duty cycle [%]" & input$secondtabset == n_factors[15]) {
      output$text2 <- renderUI({text_ACon})  } 
    
    if(input$firsttabset == "Frequency [Hz]" & input$thirdtabset == n_factors[16]) {
      output$text3 <- renderUI({text_ACfreq})  } 
    
    if (input$firsttabset == "Frequency [Hz]" & input$thirdtabset == n_factors[17]) {
      output$text4 <- renderUI({text_DCfreq})  } 
  })
  
  # ------------------------------------------------------------------------------
  # Analysis of results ----------------------------------------------------------
  
  # Import data
  data <- reactive({
    req(input$post_pro)
    validate(need(
      tools::file_ext(input$post_pro$datapath) == "csv",
      "Please upload a csv file"
    ))
    table <- table <- read.csv2(input$post_pro$datapath, sep = ";")
  })
  
  empty_plot <- ggplot() +
    geom_text(aes(x = 0, y = 0, label = "Please load data"))
  
  output$value <- renderText({
    input$model
  })
  
  fit <- reactive({
    fit <- lm(input$model, data = data())
    return(fit)
  })
  
  model <- reactive({
    if (!is_null(data())) {
      {
        lm(
          formula = input$model,
          data = data(),
          y = TRUE,
          qr = TRUE
        )
      }
      
    }
  })
  
  
  effects <- reactive({
    fit_eff <- fit()
    effects <- fit_eff$effects[2:length(fit_eff$effects)]
    return(effects)
  })
  
  residuals <- reactive({
    fit_res <- fit()
    residuals <- fit_res$residuals
    return(residuals)
  })
  
  x_factor <- reactive({
    if (!is_null(data())) {
      as.factor(data()[[input$in1]])
    }
  })
  
  tr_factor <- reactive({
    if (!is_null(data())) {
      as.factor(data()[[input$in2]])
    }
  })
  
  disegno1 <- reactive({
    graph1 <-
      text(
        qqnorm(effects(), xlim = c(-4, 4))$x,
        qqnorm(effects(), xlim = c(-4, 4))$y,
        labels = names(effects()),
        pos = 4
      )
    graph2 <- qqline(effects())
  })
  
  disegno3 <- reactive({
    graph3 <- plot(data()$Y, residuals())
  })
  
  disegno4 <- reactive({
    graph4 <- qqPlot(fit()$residuals)
  })
  
  output$qqn_plot <- renderPlot({
    disegno1()
  })
  
  output$res_plot <- renderPlot({
    disegno3()
  })
  
  output$qqp_plot <- renderPlot({
    disegno4()
  })
  
  output$boxcox <- renderPlot({
    if (is_null(model()))
      return(empty_plot)
    model()
    boxCox(model(), lambda = seq(-3, 10, 0.5))
  })
  
  # output$boxcox <- renderPlot({
  #   if (is_null(model()))
  #     return(empty_plot)
  #   model() %>% boxCox() 
  # })
  
  output$int_plot <- renderPlot({
    if (is_null(list(x_factor, tr_factor)))
      return(empty_plot)
    interaction.plot(
      x.factor = x_factor(),
      trace.factor = tr_factor(),
      response = as.numeric(data()$Y),
      trace.label = {
        as.character(input$in2)
      },
      xlab = {
        as.character(input$in1)
      },
      ylab = "Data yield"
    )
  })
  
  observeEvent(input$qqn_graph, {
    output$mplot <- renderUI({
      box(
        width = 8,
        height = 600,
        title = "Q-Q normality plot or Daniel's method",
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput('qqn_plot', height = 425),
        downloadButton('dwnplot1', 'Download Plot')
      ) #box closure
    })
  })
  
  observeEvent(input$ANOVA, {
    output$analysis <- renderUI({
      box(
        width = 8,
        height = 600,
        title = "ANOVA",
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        renderPrint({
          anova(fit())
        })
        
      )
    })
  })
  
  observeEvent(input$res_graph, {
    output$mplot <- renderUI({
      box(
        width = 8,
        height = 600,
        title = "Residuals plot",
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput('res_plot', height = 425),
        downloadButton('dwnplot3', 'Download Plot')
      ) #box closure
    })
  })
  
  observeEvent(input$qqp_graph, {
    output$mplot <- renderUI({
      box(
        width = 8,
        height = 600,
        title = "Q-Q plot",
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput('qqp_plot', height = 425),
        downloadButton('dwnplot4', 'Download Plot')
      ) #box closure
    })
  })
  
  
  observeEvent(input$bc_graph, {
    output$bc_plot <- renderUI({
      box(
        width = 8,
        height = 600,
        title = "Box-cox plot",
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput('boxcox', height = 425),
        downloadButton('dwnplot5', 'Download Plot')
      )
    })
  })
  
  observeEvent(input$int_graph, {
    output$mplot <- renderUI({
      box(
        width = 8,
        height = 600,
        title = "Interaction plot",
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput('int_plot', height = 425),
        downloadButton('dwnplot6', 'Download Plot')
      ) #box closure
    })
  })
  
  
  
  
  output$dwnplot1 <- downloadHandler(
    filename = paste("save_qqnorm_", model(), ".png"),
    content = function(file) {
      png(file, width = 1200, height = 800)
      graph1 <-
        text(
          qqnorm(effects(), xlim = c(-4, 4))$x,
          qqnorm(effects(), xlim = c(-4, 4))$y,
          labels = names(effects()),
          pos = 4
        )
      graph2 <- qqline(effects())
      dev.off()
    }
  )
  
  output$dwnplot3 <- downloadHandler(
    filename = paste("save_yieldres_", model(), ".png"),
    content = function(file3) {
      png(file3, width = 1200, height = 800)
      plot(data()$Y, residuals())
      dev.off()
    }
  )
  
  output$dwnplot4 <- downloadHandler(
    filename = paste("save_qqplot_", model(), ".png"),
    content = function(file4) {
      png(file4, width = 1200, height = 800)
      qqPlot(residuals())
      dev.off()
    }
  )
  
  output$dwnplot5 <- downloadHandler(
    filename = paste("save_bc_", model(), ".png"),
    content = function(file5) {
      png(file5)
      model() %>% boxCox()
      dev.off()
    }
  )
  
  
  output$dwnplot6 <- downloadHandler(
    filename = paste("save_int_", model(), ".png"),
    content = function(file6) {
      png(file6)
      graph6 <- interaction.plot(
        x.factor = x_factor(),
        trace.factor = tr_factor(),
        response = as.numeric(data()$Y),
        trace.label = {
          as.character(input$in2)
        },
        xlab = {
          as.character(input$in1)
        },
        ylab = "Data yield"
      )
      dev.off()
    }
  )
  
}


# Run the application
shinyApp(ui = ui, server = server)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
