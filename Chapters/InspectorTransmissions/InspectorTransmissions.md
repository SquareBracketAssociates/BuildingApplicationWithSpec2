## Transmissions inside the Inspector


In this chapter, we will explain what is a transmission, a transformation and how to have custom transmissions in the Pharo Inspector. We will show how transmissions are used in the inspector with a soccer database analysis tool that you can find at [https://github.com/akevalion/Futbol/](https://github.com/akevalion/Futbol/). We used this application because we needed a real domain to navigate within the inspector and to show some non-trivial situations where you want to abstract over the domain to get a smooth navigation flow.

In particular, the inspector is a special object presenter that control the navigation between spec presenters. And we will explain such a specific case.

So let us start by explaining transmissions.

### What is a transmission?

Transmissions are a generic way to propagate information between Spec components. They are a way to connect presenters, thinking on the “flow” of information more than the way it is displayed.

For example, let us say that we want to have a presenter with a list and with a text inside. When one clicks on the list, the text inside the text presenter will update.

Imagine we defined the layout as follows: 

```
layout := SpBoxLayout newHorizontal
     add: (list := self newList);
     add: (detail := self newText);
     yourself
```

But this does not say how list and detail are linked. We could do it by describing action to be raised on specific list events or via transmissions.

The transmission sub-framework solves this in an elegant way: Each presenter defines ”output ports” (ports to send information) and ”input ports” (ports to receive information). Each presenter defines also default input and output ports.

### Transmitting

A transmission connects a presenter’s output port with a presenter’s input port using the message `transmitTo:`` as follows:

```
list transmitTo: detail.
```

This will connect the list presenter default output port with the detail presenter default input port. This line is equivalent (but a lot simpler) to this one:

```
list defaultOutputPort transmitTo: detail defaultInputPort.
```

It is important to remark that a transmission does not connect two components, it connects two component ports. The distinction is important because there can be many ports!
Take for example the class SpListPresenter, it defines two output ports (selection and activation), this means it is possible to define also this transmission (note that we do not use the `defaultInputPort` but `outputActivationPort` instead):

```
list outputActivationPort transmitTo: detail defaultInputPort
```

Note that some presenters such as `SpListPresenter` offer selection and activation events (and ports). An activation event is a kind of more generic event. Now the selection event can be mapped to an activation event to provide a more generic layer.

### Transforming values during a transmission

The object transmitted from a presenter output port can be inadequate for the input port of the target component. To solve this problem a transmission offers transformations.
This is as simple as using the `transform:` message as follows:

```
list
     transmitTo: detail
     transform: [ :aValue | aValue asString ]
```

```
list defaultOutputPort
     transmitTo: detail defaultInputPort
     transform: [ :aValue | aValue asString ]
```

### Transforming to arbitrary input

We can also transmit from an output port to an arbitrary input receiver using the message `transmitDo:` and` transmitDo:transform:`.
It is possible that the user requires to listen an output port, but instead transmitting the value to another presenter, another operation is needed. The message `transmitDo:` message handles this situation:

```
list transmitDo: [ :aValue | aValue crTrace ]
```

### Acting after a transmission 

Sometimes after a transmission happens, the user needs to react to modify something given the new status achieved by the presenter such as pre-selecting an item, shading it…
The `postTransmission:` message handles that situation.

```
list
    transmitTo: detail
    postTransmission: [ :fromPresenter :toPresenter :value |
       "something to do here" ]
```

### Transforming transmissions inside the Inspector

We will use as an example for this chapter a football project. You can download it from here [https://github.com/akevalion/Futbol/](https://github.com/akevalion/Futbol/). There are instructions in the README of how to install it. The project allows one to visualize a european football database. Note that installing the project may take some minutes because it needs to install the database.

![Teaser of the project](figures/inspectorTransmissions1.png width=60&label=inspectorTransmissions1)

First, we can select the country league that we would like to see. In this case we choose France. Then we see a list with the seasons available. We select a season and we can see a table of points of that season of the French league.

### The problem

For calculating the statistics of a team, we use a calculator class (`TeamStatsCalculator`) and not the real model. This is a common situation in which one needs to use a helper class not to pollute the model. So, if we click on a team of the table, instead of seing a soccer team object, which is what we would expect, we see a `TeamStatsCalculator` instance. In this example, we clicked on the team LOSC of Lille but we get a `TemStatsCalculator` instead of the LOSC team (yes our team is located at Lille so we took a local team).

![Transmitting the wrong object](figures/inspectorTransmissions2.png width=60&label=inspectorTransmissions2)

What we would like to have is to transform the transmission. The class `TeamStatsCalculator` knows its real Team and it knows how to convert itself into an instance of Team with the message `TeamStatsCalculator >> asTeam`. So, we want to transform the object into an instance of `Team` and then transmit it.

Now there is a catch, as we are inside the inspector, doing the transformation is not the same as before because the inspector takes control of the presenter.

### Using SpTActivable trait

`SpTActivable` is a trait that helps us to use transmission in the Inspector.

We need to use that trait in the class of our presenter. For this example we will use the trait in the class `TablePointsPresenter`, that is our presenter class that is used inside the inspector.

```
SpPresenter << #TablePointsPresenter
    traits: { SpTActivable };
    slots: { #table . #teamEvolutionData };
    tag: 'Core';
    package: 'Football-Spec'
```

### Specifying the custom transmission

Once we user that trait, we need to define the method: `outputActivationPort` on our `TablePointsPresenter class`.

The method will say how we are going to transform the element. As we said previously, in this case, we only need to send the message asTeam to the object and it will return an instance of its real team when we click on an item of the table. To define the `outputActivationPort` method, we need to return an instance of `SpActivationPort` and specify the transform action.

For this example, we will return the output activation port of the table and to specify the transformation.

```	
outputActivationPort
 
    ^ table outputActivationPort
        transform: [ :selection | selection selectedItem asTeam ];
        yourself
```

Now, we refresh the inspector, and when we click on a row and we see that now we have the real team object for the LOSC of Lille!

![Transmitting the correct object](figures/inspectorTransmissions3.png width=60&label=inspectorTransmissions3)

### Conclusion

We saw that to transform value during a transmission in the Pharo Inspector, we only need to use the trait `SpTActivable` and to define the method `outputActivationPort`. Also, we saw how thanks to the transmissions sub-framework this work is easy to do as Spec takes care of it for us.