# Notes to Java Design Patterns

A **solid** design principle for OOP Patterns

S - Single Responsibility principle

  There should never be more than one reason for a class to change

O - Open closed principle

   Software Entities (like Classes, Modules, Methods etc.)
   should be open for extension but closer for modification

L - Liskov substitution principle

  We should be to substitute base class object with child class objects &
  this should not alter behavior / characteristics of program

I - Intervace segregation principle

  Clients should not be forced to depend upon interfaces that they do not use

D - Dependency inversion principle

   1. High level modules should not depend upon low level modules.
      Both should depend upon abstractions
   2. Abstractions should not depend upon details.
      Details should depend upon abstractions

3 types of design patterns
  creational deisgn patterns -- builder, simple factory, factory method, prototype,
                                singleton, abstract factory, object pool
  structural design patterns -- TODO! Progress in this course
  behavioral design patterns -- TODO! Progress in this course

## creational design patterns

### Builder pattern

What problem want to solve that pattern?

- Objects that need other objects or "parts" to construct them
  then buiider design pattern an help us
- In builder we remove the logic related to object construction from
  "client" code & abstract it in separate classes

Why do you implement the builder?

- We have a **complex process** to construct an object involving multible steps

Considerations need to know!

- Implementation Considerations
   1. You can easily create an immutable class by implementing builder as
      an inner static class.You'll find this type of implementation used
      quite frequently even if immutability is not a main concern.
- Design Considerations
   1. The director role is rarely implemented in a separate class,
      typically the consumer of the object instance or the client
      handles that role.
   2. Abstract builder is also not requierd if "product" itself is not
      part of any inheritance hierachy. You can directly create
      concrete builder.
   3. If you are running into a "too many constructor arguments" problem
      then it's a good indication that builder pattern may help

## Prototype

What is a Prototype?

- We have a complex object that is costly to create.
  To create more instances of such class, we use an existing instance as our prototype

What does a Prototype?

- Prototype will allow us to make copies of existing object &
  save us from having to recreate objects from scratch.

Conditions to implement a prototype for a class!

- The class must implmenet **Cloneable** interfaces
- Class should override clone method and return copy of itself
- The method should declare **CloneNotSupportException** in throws
  clause to give supclasses chance to decide on whether to support cloning

What is the Prototype registry?

- The registery provides an easy way to access frequently used prototypes.
  **It's not neccessary for the implementation of the prototype pattern**

Considerations need to know!

- Implementation considerations
  1. Pay attention to the deep copy and shallow copy of referances.
  2. Make sure to reset the mutabl state of object before returning the prototype.
     It's a good idea to implement this in method to allow subclasses to
     initialize themselves.
  3. **clone()** method is protected in Object class and must be overridden to
     be public to be callable from outside the class.
  4. A **cloneable interface** is a "marker" indicator
     that the class supports cloning.
- Design considerations
  1. Prototypes are useful when you have large objects where majority of state
     is unchanged between instances and you can easily identify that state.
  2. A prototype registry is a class where in you can register various prototypes
     which other code can access to clone out insances. This solves the issue of
     getting access to inital instance.
  3. Prototypes are usefull when you're working in composites between classes.

## Singleton

What is a Singleton?

- A singleton class has only one instance,
  accessible globally through a single point (via. method /field)
- Any state you add in your singleton becomes part of "global state" of your application
- Main problem this pattern solves is to ensure that only a single
  instance of this class exists

Implementation of a singleton

- Controlling instance creation
  - Class constructor(s) must be not be accessible globally
  - Subclassing must not be allowed
- Keeoing track of instance
  - Class itself is a good place to track the instance
- Giving access to the singleton instance
  - A public static method is good choice
  - Can expose instance as "final public static" field but
    it won't work for a singleton implementation
- Two types of implementing a singleton
  1. Early initalization - Eager Singleton
      - Create a singleton as soon as the class is loaded
  2. Lazy initalization - Lazy Singleton
      - Singleton is created when it is first requierd

## Factory Method

What is a Factory Method?

- We want to move the object creation logic from our code to a separate class.
- We use this pattern when we do not know in advance which class we may need
  to instantiate beforehand & also o allow new classes to be added to system
  and handle their creation without affecting client code.
- We let subclasses decide which object to instantiate by overriding the
  factory method.

Consideerations you need to know!

- Implementation Considerations
  1. The creator can be a concrete class & provide a default impllementation for
     the factory method. In such cases you'll create some "default" object in
     base creator.
  2. You can also use the simple factory way of accepting additional arguments
     to choose between different object types. Subclasses can then override
     factory method to selectively create different objects for some criteria.
- Design Considerations
  1. Creatore hierachy in factory method pattern reflects the product hierachy.
     We typically end up with a concrete creator per object type.
  2. Template method design pattern often makes use of factory methods.
  3. Another creational design pattern called "abstract factory" makes use of
     factory method pattern.
