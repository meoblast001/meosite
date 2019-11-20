I try to use different technologies to solve different problems when developing
software. Here is a list of notable technologies with which I've worked and how,
when, and if I prefer to use them. I've divided the technologies into
categories.

## Web Development

### Back-End

- **ASP.NET / Microsoft MVC:** I've use Microsoft MVC in traditional web
applications and in single page applications. Extensions like SignalR make
single page application development comfortable when two-way communication
occurs regularly. Entity Framework is also one of my favourite object-relational
mapping libraries becasue it leads to readable and maintainable type safe code.
Due to cross-platform support and simplifications made by Microsoft, I prefer
working with .NET Core over .NET Framework.

- **Ruby on Rails:** Ruby on Rails has helped me in both commercial and private
projects, which I previously used extensively. The extensive amount of libraries
available to extend it and its ORM ActiveRecord make it easy to write very
readable code. Despite Ruby's lack of a type system, I've had positive
experiences with the Rails framework. Unfortunately I have not had any use cases
for it since Rails 4.

- **Django / Python:** The Django web framework, along with South for
migrations, were used in private projects I worked on previously. I have not
worked on any Django projects for some time though. Microsoft MVC and Ruby on
Rails have gotten the job done just as well.

- **PHP:** Most experience I have with PHP is commercial and I tend not to use
it privately. I've worked on projects using the Yii Framework, Zend Framework 2,
and smaller projects not using any framework.

### Front-End

- **React:** React is my choice technology for developing single page
applications for the web. Its efficiency and focus strictly on UI makes it an
attractive tool. Paired with the flux pattern, I've found it easy to implement
clean solutions that run stably.

- **Angular:** While working with Angular commercially I've found it most
suitable for large single page applications. Its large feature set and the vast
amount of supporting libraries help get consistency right across an application.
I have experienced that Angular can add a lot of development overhead, so if I
use it, I must be convinced that the project can benefit from it. Otherwise
React gets the job done.

- **jQuery:** In web sites that have some advanced client-side functionality,
but not single page applications, I sometimes use jQuery to express verbose
JavaScript operations with less code. Due to developments in browser technology
and the shift toward single page applications though, I now
tend to use jQuery less frequency.

### Miscellaneous Web-Dev

- **Hakyll:** I have written websites, including this one, with the static site
generator Hakyll. The frameworks simplifies building static websites and can
convert various source resources to their publish format.

## Game Development

- **Unity:** If I'm making a game, it's easiest for me to turn to Unity. It's
the engine with which I have the most experience, it makes prototyping easy, it
supports almost every feature I need (and the rest is on their roadmap). I've
worked with the asset pipeline, built UIs, have made both 2D and 3D scenes,
synchronised multiplayer games using the (now deprecated) high level networking
API, and have prototyped levels with ProBuilder. Currently I've only worked with
Unity in a private context. Look at my projects page to see what I've done with
it.

- **Godot:** Because I prefer free and open source software, I naturally learned
and used Godot. Currently I am more experienced with Unity but have used Godot
for 2D game development. Look at my projects page for an example.

## Desktop Graphical User Interface Development

- **Qt:** When writing a graphical desktop application I prefer using Qt, either
with Qt Widgets or QML. It's cross-platform, leads to fairly clean C++ code, and
supports most commonly used graphical control elements. I've used it in both
private and commercial contexts. In additional to desktop applications I've also
used it for applications deployed on mobile devices.

## Mobile Development

- **Android:** I've used the Android SDK for developing Thugaim, a game which
can be found among my projects. My experience has been mostly limited to
activities, the API for working with rendering to canvases, and input. My
experience with Android does not extend beyond game development though.
