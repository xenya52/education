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

- We have a complex process to construct an object involving multible steps

Considerations need to know!

- Implementation Considerations
   You can easily create an immutable class by implementing builder as
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
