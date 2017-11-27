# wh40ksim
Warhammer 40k Combat simulator

+++ Begin encrypted transmission +++

To: Shas'O Vior'la Shovah Kais Mont'yr  
From: a friend in the old home.  

My very dearest O'Shova,

A friend of ours in the Earth caste has forwarded me information on a new device
designed by our Engineers, which they call a Combat Emulator. The Fire caste has
been using it to great effect to anticipate the actions of our enemies in combat
and have already earned many accolades for their increased effectiveness in
battle.

Although the device is still a mere prototype (the Earth caste friend says it's
"still in beta") it can already simulate the outcome of a set of units firing
upon a single target, while taking into account both sides' movement and even
cover, to some degree. A full battlefield simulation is still a long way away,
but other aspects of combat are in the works. Our engineers sensibly considered
barbarous melee fighting not to be the primary objective. Some races' primitive
superstitions regarding "psychic powers" will eventually have to be taken into
account also- they often seem capable of turning the tide of battle, therefore,
we must be prepared.

The device still lacks significant data on most of our enemies, or even our own
troops. The Earth caste is working to include more of our existing intelligence
but for the time being, only a few of our Fire caste troops are represented and
just a handful of our enemies, specifically a couple of types of units of the
Imperium of Man. I understand you've had some close encounters with their sort
recently, so I decided to forward the schematics of the Combat Emulator to you,
just in case you might find some use for it. 

Unfortunately, I could not secure any documentation regarding the operation of
the device, however I expect you will want to pass on the schematics to a
certain venerable Earth caste colleague of yours, who will be perfectly capable
of understanding how to use, and perhaps even begin improving, the Emulator. For
my part, I will make sure to keep you up-to-date with any new additions to the
device from the Engineers over here.

For the Greater Good  
Your old comrade in arms  
S.O.S.

+++ End encrypted transmission +++


Actual real world documentation, such as it is
==============================================

This is a WH40K simulator, still in beta and woefully incomplete. I'm supposed
to be doing real work and people will complain if I spend all my time on this,
so updates will have to be relatively slow.

Motivation
----------

Let's take it from the start. This "simulator" aims to become a tool to help
answer speculative tactical questions such as "what happens if I engage a
Hormagaunt brood in melee with my Fire Warriors?" or, something perhaps
something a little harder to guess like "How long does it take to shoot down an
entire squad of Terminators with a unit of gun drones"? 

As an aside, you'll notice that my examples focus heavily on the T'au Empire.
That's because I got a T'au army and I haven't really played WH40K for a few
years, so my breadth of knowledge on other armies is rather limited.

To continue with the goals of the project, I figure the best way to answer such
questions is -well, the best way is to actually play the battles and see what
happens. But, of course, playing WH40K involves rolling dice and just by playing
a couple of battles you may not be able to tell whether the dice have rolled in
your favour or your tactical decisions were sound. To really be sure, you'd have
to play *the same battle* many, many times in a row and record the results. If
you really wanted to be thorough about it, you would to decide on a set of
competing strategies in advance, then follow those strategies to the letter,
repeating each game with a given strategy a number of times. 

For instance- say you wanted to find out what happens when your Fire Warrior
Strike Team with a Shas'ui and two gun drones in tow, shoot at a Tactical Squad
of 9 Space Marines and a Sergeant, while the Marines advance every turn, all the
while shooting from the hip, trying to get to your T'au and slaughter them in
hand-to-hand.

Obviously, the outcome of this "scenario" would vary significantly between
multiple runs, because of the dice- but also because we're only human and we
don't easily stick to our detailed plans laid out in advance. For example, you
might forget to resolve the gun drones' shooting in a turn, or maybe you'd get
bored playing the same battle 40 times in a row and add a couple of Marines with
Heavy Weapons, or a couple of shield drones, to spice things up... invalidating
your data gathering in the process.

In fact, the tedious repetition of the same game over and over again is the main
factor that makes this sort of data gathering exercise a virtual impossibility
in the real world. On the other hand, computers don't get bored. Provided a
sufficiently complete combat simulator, there's nothing stopping us from running
our battle-scenario a thousand, ten thousand, ten million times in a row (well-
nothing except the fact that it's going to be a bit expensive, but I digress).

This is the reasoning behind this simulator: it allows you to choose a few
models on either side and find out what happens when they shoot at each other
-and, eventually, what happens when they start fighting or clobbering each other
with psychic powers. For the time being, only shooting is supported (again,
because I have a T'Au army and the T'Au don't do melee, if they can avoid it and
they don't have any psykers).

Limitations of the implementation
---------------------------------

It should be noted carefully that in order for a simulator like this to be
practicable, a number of concessions have to be made. In particular, a complete
simulation of an entire battle is completely out of the question, or, at least,
out of the scope, of this project. Such a complete simulation would have to
allow for an arbitrary number of units, on an arbitrary board. The project would
have to simulate every possible terrain detail accurately and in general its
complexity would rise to be on par with a complete WH40K video game, albeit
without the graphics. This is not a trivial undertaking.

This is not to say that such a simulator can't be created- sure it can. But I'm
just one poor and lonesome PhD student working on this in my spare time and with
my supervisor breathing down my neck to finish writing that damn papers, *now*!
So there's not the time or the resources to do the entire thing. At least, not
right now.

For the time being, I think it would still be very useful to be able to predict
the outcome of specific tactics, even limited in scope to only two sets of
units, removed from a larger battle. And this is what this project aims to
achieve.

Additionally, the limitations of this combat simulator at this early stage of
its development go well beyond its limited scope. Primarily, there are two
issues to keep in mind:

1. Data is sparse because obviously I don't have every WH40K 8th edition Index
   in my disposal and even more so, I wanted to focus on the simulation first
   and I only needed data for a few units to do that.
   
   You can add data on more units in the datasheets module, but to do that you
   would have to understand the language used in the simulator, Prolog. This is
   notoriously hard to do, even for experienced soft. engineers. At some point I
   will make this process more available, even to the non-technical. But I
   haven't gotten around to that yet.

2. There is still very little functionality to help create a unit, with wargear
   etc selections and all. All you can do for the time being is select the types
   and numbers of models you want to go into a unit- and you won't even get a
   selection of weapons, they just get what happens to be the first weapon in
   the unit's listing in the datasheets module.

   This too shall pass, but it will take some time before I get to it.

The project started with an idea about creating pretty little plots to show the
outcome of simulations graphically, because that is always nice to have. This is
another thing I haven't gotten around to yet. For now, you can look at the
glorious textual output of simulations in the Swi-Prolog console. It might hurt
your eyes a bit but at least you can see that something is going on.

Finally, the Swi-Prolog console is all you have to setup the simulations you
want. It's not a big deal to make it possible to compile army lists and draw up
combat scenarios from a file, but ... well, I haven't gotten round to it yet.

A bit of help to setup and use the project follows, below.

How to actually run this thing and experiment with it
=====================================================

Setting up and running the WH40K simulator
------------------------------------------

This project was developed with Swi-Prolog 7.7.3. You will need to have this, or
a higher version, installed on your machine to get it to run. See the Swi-Prolog
installation instructions on the Swi download page, below:

http://www.swi-prolog.org/download/stable

Once Swi is installed, you will obviously need to get the sources for the
project from this repository. You can use the "Clone or download" button near
the top of this page to copy a correct clone url, then enter something like the
following in your console:

```
git clone https://github.com/stassa/wh40ksim.git
```

This will create a new directory called wh40ksim in the location where you run
the command from. You're reading this on github, so you probably know all about
cloning repositories already.

Prolog is an interpreted language so you don't have to worry about building the
project, or anything like that. Once you have the sources, if you're on a
windows machine, you can start the project up by double-clicking on the project
load file, at the top level of the project sources:

```
load_project.pl
```

This will start the Swi-Prolog console (a command line where you can enter
Prolog queries) and bring up the Swi-Prolog IDE (a text editor where you can
browse, and modify, the source).

If you're on a Linux box double-clicking the project load file won't do
anything. Instead, you will have to navigate to the project's top-level
directory in your console, start Swi-Prolog (if you've set up everything in a
sensible manner, the console would start with "swipl") and consult the project
load file from inside the Swi command line. To consult the file, enter the
following in the Swi console:

```
[load_project.pl].
```

This will do the same thing as double-clicking the project on windows. That is,
if you're on a graphical X server, it will bring up the IDE. If you're in text
mode, it will fail and you're on your own after that. Then again, if you're in
text mode you don't need me to hold your hand here.

Units and model-sets 
---------------------

The simulator makes use of a concept of "model-sets", which is not something
you're likely to have found in any official Games Workshop™ documentation.
Indeed, it's just a term I pulled out of my backside (the same place the sun
shines form every morning). On the other hand, it's very convenient.

A model-set S is defined as a subset of the models in a unit U where all the sᵢ
∈ S have the same profile p _and wargear W_: 
```
S : { s ∈ U | profile(s) = p, p ∈ P ∧ wargear(s) = W, W ⊆ Wg }
```

Where P the set of profiles, Wg the set of wargear items equipped by models in
U.

The perceptive user will notice that the model-set definition above defines a
partitioning relation for sets of models. Just thought you might be interested
to know that we're doing proper science, here. I know, right?

Using this definition, sub-sets of models in a unit with diverse models can be
treated as one for the purposes of a simulation. 

For instance, consider a T'Au Empire Tactical Drone unit consisting of 2 MV1 Gun
Drones, an MV4 Shield Drone and an MV7 Marker Drone. The gun drones are armed
with Pulse Carbines, the Marker Drone with a Markerlight and the Shield Drones
have no weapons. Say that we want to have the unit fire against a target. We
need to figure out such details as how many attacks the unit can make, the Power
and Armour Piercing stats of the attack, its maximum Range etc. This information
is different for each type of model in the unit and we would consequently have
to determine it separately for each model. On the other hand, all these stats
depend on the profiles and wargear selections of models. Therefore, if we split
the unit to sub-sets of models, all of which have the same wargear and profile,
we can safely treat each of those "model-sets" as one for the purposes of
combat.

This indeed is pretty much the same thing as the usual "fast die rolling"
techniques described in most Warhammer™ rulebooks.

With this in mind, this is how to assemble a unit in the simulator and
partition it into model-sets: 

```
models_unit([fire_warrior_shasui-1, fire_warrior-7, mv1_gun_drone-1, mv36_guardian_drone-1], 'Strike Team 1', _N-_Us), model_sets(_Us, _Ss), forall(member(Si,_Ss),(writeln('Model-set:'),forall(member(Sk,Si),writeln(Sk)))).
```

Entering the above query in the Swi Console will result in the following output:

```
?- models_unit([fire_warrior_shasui-1, fire_warrior-7, mv1_gun_drone-1, mv36_guardian_drone-1], 'Strike Team 1', _N-_Us), model_sets(_Us, _Ss), forall(nth1(I,_Ss,Si),(format('Model-set ~w:~n', [I]),forall(member(Sk,Si),writeln(Sk)))).
Model-set 1:
model(mv36_guardian_drone,8,+ 5,+ 5,3,4,1,1,6,+ 4,guardian_field-1)
Model-set 2:
model(mv1_gun_drone,8,+ 5,+ 5,3,4,1,1,6,+ 4,pulse_carbine-2)
Model-set 3:
model(fire_warrior_shasui,6,+ 5,+ 4,3,3,1,1,7,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
true.
```

If you entered the entire text, with the "?-" at the start you will probably see
an error message. In the future, when you see an example query with output like
the one above, omit the "?-" when entering it in the Swi-Prolog console.

The first predicate, models\_unit/3 takes as arguments a list of models and
their numbers (in the form Model-Numer, separated by a hyphen) and a name for
the unit and expands this list to a list including the specified number of
models the specified number of times. In this unit-list, each model is a Prolog
term model/11, that holds the model's profile information and wargear options.

As discussed earlier, the functionality for assembling a unit is still
rudimentary and you can't select specific wargear items- you only get what ever
happens to be listed first in the datasheets.pl file entry for the unit type (as
clauses of the predicate unit\_profiles/11. For those who don't know Prolog but
know SQL, this is basically the same as saying there's a table called
"unit\_profiles" with 11 columns and row for each unit profile known to the
system).

The next predicate call, model\_sets/2 takes as argument a unit, created from
models\_unit/3 and partitions it to model-sets, of all models with the same
profile and wargear.

Rollouts and simulations
------------------------

This project uses the term "rollout" to mean a "static" simulation of a single
combat sequence (shooting, fighting, etc, though only shooting is implemented
for the time being). A rollout is "static" in the sense that any parameters such
as distance of the attacker to its target, the attacker's movement etc, are
given from the start and do not change throughout the rollout. Namely,
casualties are not removed from the target unit (although casualties are
reported).

Rollouts are meant to be used to inform of the probability to inflict casualties
when attacking one unit with another. For best results, multiple rollouts of the
same combat situation should be executed.

The following example shows how to run 1000 rollouts of a single instance of
shooting, reporting the results (and also the time taken) at the Swi-Prolog
console:

```
?- _N= 1000, _Parameters = [18, none, 0], models_unit([mv1_gun_drone-12], 'Tactical Drones 1', _N1-_Us1), model_sets(_Us1, _Ms), models_unit([space_marine_sergeant-1, space_marine-4], 'Tactical Squad 1', _N2-_Us2), time(n_rollouts_report(_N, shooting, [_Ms, _Us2, _Parameters])).
Completed 1000 shooting rollouts
Average number of survivors per rollout: 2.193
% 4,903,899 inferences, 1.125 CPU in 1.138 seconds (99% CPU, 4359021 Lips)
true.
```

The list Parameters = [18, none, 0] is a specification of the rollout parameters
which as discussed above do not change throughout the experiment. 

The first parameter, "18" is the distance of the attacker to the target, so all
rollouts will calculate the results of the attacking unit shooting while 18"
from their target. 

The second argument, "none" is a description of the movement of the attacker in
the last movement phase: in this case, it means the unit remained stationary.
The full list of movement types recognised by the simulator are:

* none
* standard
* advance

And that's all for now. 

The last argument, "0" refers to the cover bonus of the target- in this case,
the target is not in cover, so they have a 0 bonus to their saves.

The 1000 rollouts do make it clear that a single round of shooting with 12 gun
drones at a Tactical Squad of 6 marines in close range is lethal. But what
happens if the drones had to *advance*, before shooting? In this case, they have
a -1 penalty to their to-hit rolls and we can simulate this with a slightly
changed list of parameters:

```
?- _N= 1000, _Parameters = [18, advance, 0], models_unit([mv1_gun_drone-12], 'Tactical Drones 1', _N1-_Us1), model_sets(_Us1, _Ms), models_unit([space_marine_sergeant-1, space_marine-4], 'Tactical Squad 1', _N2-_Us2), time(n_rollouts_report(_N, shooting, [_Ms, _Us2, _Parameters])).
Completed 1000 shooting rollouts
Average number of survivors per rollout: 3.31
% 4,094,979 inferences, 1.031 CPU in 1.030 seconds (100% CPU, 3970889 Lips)
true.
```

It's clear to see that advancing has a slight negative effect on the drones'
accuracy. What if we also assumed that the marines are in cover?

```
?- _N= 1000, _Parameters = [18, advance, 1], models_unit([mv1_gun_drone-12], 'Tactical Drones 1', _N1-_Us1), model_sets(_Us1, _Ms), models_unit([space_marine_sergeant-1, space_marine-4], 'Tactical Squad 1', _N2-_Us2), time(n_rollouts_report(_N, shooting, [_Ms, _Us2, _Parameters])).
Completed 1000 shooting rollouts
Average number of survivors per rollout: 4.15
% 4,112,552 inferences, 1.094 CPU in 1.101 seconds (99% CPU, 3760048 Lips)
true.
```

Now the marines can survive the onslaught a lot better. Morale of the story:
always use terrain to your advantage.

As discussed, rollouts only make use of static, unchanging parameters. Their
dynamic counterpart are "simulations". These make use of a "scenario" that
describes how the target and attacker move around the battlefield and also
tracks casualties inflicted to the target every turn.

Here is an example of running a simulation, complete with a simple scenario:

```
 _Sc = [turns(6), starting_distance(36),attacker_movement(none,0),target_movement(advance,-1),target_cover(false,_)], _U1 = [mv1_gun_drone-12],  _U2 = [space_marine_sergeant-1, space_marine-9], scenario_simulation(shooting, _Sc, _U1, _U2, _Rs), forall(member(Ri,_Rs),writeln(Ri)), length(_Rs, Survivors).
Sim ends after 4 turns with attacker -3" from target
model(space_marine,6,+ 3,+ 3,4,4,1,1,7,+ 3,boltgun-1)
model(space_marine,6,+ 3,+ 3,4,4,1,1,7,+ 3,boltgun-1)
model(space_marine,6,+ 3,+ 3,4,4,1,1,7,+ 3,boltgun-1)
Survivors = 3.
```

The list Sc = [turns(\_6), starting\_distance(36), attacker\_movement(none,+1),
target\_movement(advance,-1), target\_cover(false,\_)] defines a "scenario", a
set of parameters that the simulation will update dynamically, as it goes on.

A scenario list must be in a specific order and must contain the same paremeter
terms, although their values may vary. The order and terms are as follows:

1. **turns(N)**  
   The number of turns to simulate. N should be an integer, greater than 0.

2. **starting_distance(D)**  
   The distance of the attacker to the target at the start of the simulation. D
   should be an integer, greater than 0.

   In each turn of the simulation, the distance of the attacker to the target
   will be updated from the starting distance according to the target and
   attacker's movements.

3. **attacker_movement(T,O)**  
   The type, T and orientation, O, of the attacker's movement.

   T can be one of: {none, standard, advance, fall\_back}. Note that the dynamic
   simulation will recognise "fall\_back" as a movement type which static
   rollouts currently do not. This is just a bug. Don't worry, I'll squish it.

   O is one of [-1, +1]. "-1" means the attacker is moving _towards_ the target,
   "+1" means it is moving _away_ from the target.

   As a mnemonic, think of the negative sign as meaning the distance between the
   two units is reduced, a positive sign meaning the distance is increased.

   You can specify a different orientation value, specifically any positive or
   negative integer- but that will just give you silly results. Another bug for
   the squishing.

   Note that if movement type is "T" it doesn't matter what you enter as the
   orientation- the unit will always move exactly 0". To avoid errors however,
   make sure there is a number there (so don't enter an underscore).

4. **target_movement(T, O)**  
   The type, T and orientation, O, of the target's movement.

   These are the same as for the attacker's movement. Specifically, the meaning
   of the orientation values _is not flipped_. A "-1" still signifies that the
   unit is moving _towards_ its adversary, whereas a "+1" that it is moving away
   from it.

5. **target_cover(C,V)**  
   Whether the target unit is benefiting from cover and the resulting cover
   bonus it receives to its Save rolls.

   The second argument, V should always be left unbound- the simulation will
   fill in the blanks. You can use an underscore "\_" in that position.

   C should be either a boolean, in [true, false], or a number given as a
   positive modifier, in the format "N+" where N a number from 1 to 100.

   If C is the atom "true", V is 1 (the unit has a +1 modifier to its save
   rolls).

   If C is the atom "false", V is 0 (no bonus to saves).

   If C is a positive modifier, the simulation will determine for each turn
   whether the target finds, and benefits from, cover _in that turn_. The value
   of the modifier is the probability, out of 100, that this will occur.

   For instance, specifying "target\_cover(30, \_)" means that the target will
   have a 30% chance to find cover in each turn of the simulation. Each turn,
   the simulator will roll a 100-sided die and if this rolls under, or equal to,
   30, it will grant a bonus of 1 to the target, otherwise 0.

As everything else in the project, the dynamic simulation itself is still not
very flexible and it's only possible to define very simple scenarios. For
instance, it doesn't allow units to react dynamically to their opponents'
behaviour (for example, to seek cover when taking too heavy casualties etc). The
target can't shoot back, just advance and take it like a man. And so on.

Creating permanent army lists and defining combat simulation files
------------------------------------------------------------------

In short, this too is not yet an option. You can only enter queries at the
Swi-Prolog console. This is just a matter of me finding the time to add that to
the project.

In the meantime, here's some more things you can do at the Swi Console.


**Calculate number of attacks for each model-set in a unit:**

```
?- models_unit([mv1_gun_drone-12], 'Tactical Drones 1', _N-_Us), model_sets(_Us, _Ss), member(_Si, _Ss), model_set_attacks(_Si, _M, _Wn, _Pa, _Wa), number_of_attacks(_M, _Pa, _Wa, _Wn, Attacks).
Attacks = 48.
```

**Roll to hit for each model-set in a unit:**
```
?- models_unit([fire_warrior-7, fire_warrior_shasui-1, mv1_gun_drone-2, mv36_guardian_drone-1], 'Tactical Squad 1', _N-_Us), model_sets(_Us, _Ss), forall( nth1(I, _Ss, Si), ( model_set_attacks(Si, _M, _Wn, _Pa, _Wa), number_of_attacks(_M, _Pa, _Wa, _Wn, _A), hit_roll(_A, (5+), 0, Hn), format('Model-set ~w:~n', [I]), forall(member(Mi,Si),writeln(Mi)), format('hits ~w times in ~w attacks.~n', [_A,Hn]) ) ).
Model-set 1:
model(mv36_guardian_drone,8,+ 5,+ 5,3,4,1,1,6,+ 4,guardian_field-1)
hits 0 times in 0 attacks.
Model-set 2:
model(mv1_gun_drone,8,+ 5,+ 5,3,4,1,1,6,+ 4,pulse_carbine-2)
model(mv1_gun_drone,8,+ 5,+ 5,3,4,1,1,6,+ 4,pulse_carbine-2)
hits 8 times in 2 attacks.
Model-set 3:
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior,6,+ 5,+ 4,3,3,1,1,6,+ 4,pulse_rifle-1)
model(fire_warrior_shasui,6,+ 5,+ 4,3,3,1,1,7,+ 4,pulse_rifle-1)
hits 8 times in 2 attacks.
true.
```

**Roll to wound models in a target unit:**
```
?- models_unit([mv1_gun_drone-12, mv36_guardian_drone-1], 'Tactical Squad 1', _N-_Us), wound_roll(8, 4, 4, Wounds).
Wounds = 2.
```

**Allocate wounds to models in a unit:**
```
?- models_unit([mv1_gun_drone-12, mv36_guardian_drone-1], 'Drone Squad 1', _N-_Us), allocate_wounds(10, _Us, _Ms), forall(member(Mi-Ws, _Ms), (model_value(Mi,'W',Wi), model_value(Mi,name,Nm), format('Allocated ~w wounds to ~w with ~w wounds remaining~n', [Ws, Nm, Wi]))), configuration:wound_allocation_strategy(_WAS), format('Wounds allocated by strategy ~w~n', [_WAS]).
Allocated 0 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 0 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 0 wounds to mv36_guardian_drone with 1 wounds remaining
Allocated 1 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 1 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 1 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 1 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 1 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 1 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 1 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 1 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 1 wounds to mv1_gun_drone with 1 wounds remaining
Allocated 1 wounds to mv1_gun_drone with 1 wounds remaining
Wounds allocated by strategy fewer_wounds_first
true.
```

Yes, in theory you can configure what "wound allocation strategy" you want to
employ. Sure, in theory you can even define your own (although you'll need to
learn Prolog to do so).

No, in practice, there is only one wound allocation strategy currently possible:
those models with the fewer wounds left are first in line for wounds to be
allocated to their unit. This is a bit meh, but like I keep saying, we're still
in beta, ja?

**Roll to save for each model in a set of models** to which wounds have been
allocated taking into account attacker's AP and defender's cover bonus and
report remaining unsaved wounds:
```
?- models_unit([mv1_gun_drone-12, mv36_guardian_drone-1], 'Tactical Squad 1', _N-_Us), allocate_wounds(10, _Us, _Ms), saving_throws(_Ms, -2, 1, _Fs), forall(member(Fi-Sv,_Fs), (model_value(Fi,name,Nm), format('~w failed ~w saves~n',[Nm,Sv]))).
mv1_gun_drone failed 0 saves
mv1_gun_drone failed 0 saves
mv36_guardian_drone failed 0 saves
mv1_gun_drone failed 1 saves
mv1_gun_drone failed 0 saves
mv1_gun_drone failed 1 saves
mv1_gun_drone failed 1 saves
mv1_gun_drone failed 1 saves
mv1_gun_drone failed 1 saves
mv1_gun_drone failed 0 saves
mv1_gun_drone failed 0 saves
mv1_gun_drone failed 0 saves
mv1_gun_drone failed 1 saves
true.
```

**Inflict damage and modify models' wounds accordingly**:
```
?- models_unit([mv1_gun_drone-10], 'Tactical Drones 1', _N-_Us), allocate_wounds(10, _Us, _Ms), saving_throws(_Ms, -1, 0, _Fs), inflict_damage(_Fs, 3, _Rs), forall(member(Ri, _Rs), (model_value(Ri, 'W', Ws), model_value(Ri,name,Nm), format('Model ~w has ~w wounds remaining~n', [Nm,Ws])) ).
Model mv1_gun_drone has -2 wounds remaining
Model mv1_gun_drone has 1 wounds remaining
Model mv1_gun_drone has -2 wounds remaining
Model mv1_gun_drone has 1 wounds remaining
Model mv1_gun_drone has -2 wounds remaining
Model mv1_gun_drone has -2 wounds remaining
Model mv1_gun_drone has -2 wounds remaining
Model mv1_gun_drone has -2 wounds remaining
Model mv1_gun_drone has -2 wounds remaining
Model mv1_gun_drone has 1 wounds remaining
true.
```

**Simulate one round of shooting, start to end, reporting survivors in the
target unit:**
```
_Parameters = [15, standard, 0], models_unit([fire_warrior-11, fire_warrior_shasui-1], 'Strike Team 1', _N-_Us), model_sets(_Us, _Ms), models_unit([space_marine_sergeant-1, space_marine-4], 'Tactical Squad 1', _N2-_Us2), shooting_sequence(_Ms, _Us2, _Parameters, _Ss), forall(member(Si, _Ss), writeln(Si)), length(_Ss, Survivors).
model(space_marine,6,+ 3,+ 3,4,4,1,1,7,+ 3,boltgun-1)
model(space_marine,6,+ 3,+ 3,4,4,1,1,7,+ 3,boltgun-1)
model(space_marine_sergeant,6,+ 3,+ 3,4,4,1,2,8,+ 3,boltgun-1)
Survivors = 3.
```

In this case, a scenario is not needed (we're only simulating a single round of
shooting). However, the parameters for that round are still required and given
in the list: Parameters = [15, standard, 0], meaning the fire warriors are
shooting from 15" away, after a standard move and against a target outside of
cover.

Thought for the day: Good things come to those who wait.
--------------------------------------------------------

If you wait long enough I'll probably end up fleshing out this silly little
project with more bells and whistles, until you can actually make use of it.

But heed my warning: a simulation is just a simulation. At best, you can hope to
learn what are the probabilities of different outcomes to your actions. How a
real game will actually turn out cannot be predicted in advance. If it could, it
wouldn't be fun, now would it?
