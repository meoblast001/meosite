---
title: Global Game Jam 2020
identifier: global_game_jam_2020
---

The weekend of the 1st and 2nd of February concluded the Global Game Jam, a world-wide event where teams create a video
game in a weekend. Any city can register a site for the event and anyone can join. Because I live in Dresden, where I
was unable to find a jam site, I registered for the Leipzig site at HTWK. Unfortunately this meant that I would arrive
late and leave early, because I needed to take a train on Friday and Sunday evening to get there and home.

The global theme was "repair", which means every game at every site needs to incorporate this topic into the game in
some way. By the time I joined my team there were already candidates for game ideas. Our final decision was ambitious,
but was a great opportunity to learn.

### The Game Idea

Our game idea was to incorporate a classic PC game with a virtual reality (VR) device to create cooperative
multiplayer. The PC player is a robot that is fleeing from rockets. Parts of the robot take on damage and this affects
the player's ability to control the robot. The VR player is a drone that repairs problems in the robot's parts. To
encourage cooperative gameplay, only the robot can see the health status of parts, and only the drone can see from what
direction oncoming missiles are coming. Missiles damage the robot, but if it reacts fast enough, it can use a shield
pointed in the direction of the missile to block it.

### The Technology Stack

Our team decided to use the Unity game engine because all team members have experience with it. We used some additional
extensions for Unity, including the experimental new input system, Zenject, and FMOD. To enable VR we also used the
Oculus Integration package from the asset store. This gave us quite a few opportunities to learn something new.

I was one of two programmers in a team of five. We split our tasks, so I did not get much experience with Oculus
Integration and FMOD. It was enjoyable to see the world we created through VR glasses though, considering that I haven't
used much VR before the game jam.

My tasks primarily involved the new input system and Zenject, so this is where I had the opportunity to learn the most.
I will cover what I learned in the next few sections.

#### Unity's New Input System

Since version 2019.1, Unity began releasing a package that uses a different technique for accessing player input. In the
past there was a list of "axes", that could either represent a joystick, buttons, keys, or a mouse. The developer needed
to know the name of these axes and type them into a list mapping them to actions. This worked quite well if you were
only using keyboard and mouse, but for gamepads it was complicated if you needed to support more than one kind of
gamepad. It was much more difficult if you had multiplayer with multiple gamepads.

Unity's new input system no longer has a global list of actions, but allows you to create an asset including input
actions and their settings. Any game script just needs to know what input actions asset it should use. The input actions
asset sets the actions, the types of values they provide, and what input devices these values come from. The programmer
just works with the actions and the values, and doesn't need to know its source. Describing what inputs to use is also
much more intuitive and allows one configuration to cover an entire class of input devices.

The new input system is activated as a package from the package manager. It is currently marked as preview, but I've
noticed few bugs so far, which makes it perfect for a game jam. And although I've used the new input system before, I
haven't with a project with as many inputs as this one, making the decision even better.

#### Zenject

Zenject is an asset that can either be installed through the Unity asset store or as a package from its GitHub releases
page. It brings not only dependency injection to Unity but also a message bus. Before this game jam I've only tried the
most basic functionality of Zenject, so using it in a real project was a nice experience. As far as scripting went, to
inject something you only need a C# attribute on the member variable. Then to configure how injections are done for each
type you create an installer script and attach that to a Zenject context. If set up properly, the rest is fairly easy to
use.

We had an opportunity to use Zenject's signals and this feature was very useful. In almost every Unity project in which
I've participated, some message bus needed to be made from scratch to communicate game state changes around the game.
Because Zenject offers this, and with a pretty clean API, it's was better just to use what they offer.

The combination of dependency injection, signals, and some C# API designing resulted in health statistics that could
simply be injected into a game component in one line, and any property changed in this object would automatically
propogate across the game. That was cool!

#### Oculus Integration

Another developer on our team was mostly responsible for developing VR parts of the game. Although I didn't directly
get much experience integrating Oculus devices into the Unity game, I did have the chance to indirectly learn about it
because game jams are social events and the VR developer explained some of it to me.

The other team members that have experience with VR preferred using the Oculus Integration asset in the Unity asset
store, and I did get the impression this makes the integration a lot easier.

The most important things to support were movement and use of the controllers. Updating cameras and objects to position
themselves where the VR glasses and controllers are was as simple as adding some prefabs and components from the Oculus
Integration asset. Button presses were handled using Unity's input system.

### The Results

Unfortunately we did not have a final product for this game jam. We have put some effort in after the game jam though
and hopefully we can create some playable game. In any case though, it was an opportunity to learn a lot and I invite
anyone to look at the code on GitHub if they want to learn more.

### Links

* [Game Jam Project on GitHub](https://github.com/meoblast001/GlobalGameJam2020Leipzig)
* [Global Game Jam](https://globalgamejam.org/)
* [Blog Post on New Unity Input System](https://blogs.unity3d.com/2019/10/14/introducing-the-new-input-system/)
* [Zenject](https://github.com/modesttree/Zenject)
* [Oculus Integration for Unity](https://assetstore.unity.com/packages/tools/integration/oculus-integration-82022)
