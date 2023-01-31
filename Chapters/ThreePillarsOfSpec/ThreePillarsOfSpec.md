##The dual aspects of presenters: Domain and interaction model 
@cha_fundamentals_of_spec 


In this chapter we visit the key aspects of Spec and put the important 
customization points of its building process in perspective. 
We start by presenting an important aspect of Presenters: the way they handle communication with domain objects that here we call a model.

### About presenter on a model

It is frequent that you want to open a presenter on a given object.
In that case you would like that the sub presenters \(list, text,..\) get initialized based on the object that you passed. 
For example, you may want to get all the items of your basket.

However simply creating a presenter using the message `new` and passing the object will not  work because the messages such as `initializePresenters` will be already sent. 

There are two ways to address this situation in Spec and in particular Spec offers a special presenter called `SpPresenterWithModel`. This small chapter explains how to take advantage of it. 

We will build the simplest example to show the way to do it. We will implement a presenter that lists the method signatures of a class, first using a presenter and second using a presenter with model. 

### With SpPresenter

If you do not need to react to model changes, you can simply inherit from `SpPresenter`, override the `setModelBeforeInitialization:` method to set your domain object and use `YourPresenter on: yourDomainObject` to instantiate it.

This is exactly what we do here after.

First we create a new presenter class.

```
SpPresenter << #SpMethodLister
    slots: { #aClass . #list};
    package: 'Spe2Book'
```

We define a list presenter and populate it. 

```
SpMethodLister >> initializePresenters 
    list := self newList.
    list items: aClass selectors sorted
```

We assign the argument of the `on:` message to the internal instance variable `aClass` for future use. 
```
SpMethodLister >> setModelBeforeInitialization: aModel
    aClass := aModel
```

We define a basic layout of the list presenter.

```
SpMethodLister >> defaultLayout
    ^ SpBoxLayout newTopToBottom add: #list ; yourself
```

The following snippet creates a window with the list of methods of the class `Point` as shown in Figure *@pointselectors@*.
```
(SpMethodLister on: Point) open.
```

![A simple list of sorted selectors of the class Point.](figures/PointSelectors.png label=pointselectors&width=50)


### Presenter vs. PresenterWithModel

The key difference with using `SpPresenter` and `SpPresenterWithModel` is 
if you need to react to changes of the model. If you need, use `SpPresenterWithModel`.

By reacting to model changes, we means that while the presenter is open, an event changes the model 
that was used to build the UI. 
The following snippet shows that the change of model is not taken into account in the sense that the list 
is not refreshed and still display methods of the class `Point`, while the methods of the class `Rectangle should be displayed.

```
| lister | 
lister := SpMethodLister on: Point.
lister open.
lister class: Rectangle
```


### With SpPresenterWithModel

A presenter may also have a model that is a domain object you need to interact with to display or update data. 
In this case, you should inherit from `SpPresenterWithModel` so that the presenter keeps a reference to the domain object and manages its change.
As a client of this presenter we use the message `model:` to change the model. 

The corresponding method is inherited from the superclass. This `model:` method implements the following behavior: 
If the domain object is an instance of `Model`, it is stored as is in the presenter, else a value holder is created to hold the domain object so that you can be notified when the domain object used by the presenter changes. You do not need to define the method `setModelBeforeInitialization:`. 

Let us look at our little example. First we inherit from `SpPresenterWithModel`.

```
SpPresenterWithModel << #SpMethodListerWithModel
    slots: { #list };
    package: 'Spe2Book'
```

Second we define the specialize the message `initializePresenters`.
Note that if you plan to react to change it is better to define a part of the initialization in the method `modelChanged` as follows: 

```
SpMethodListerWithModel >> initializePresenters 
    list := self newList.
```

You can then implement the `modelChanged` method to refresh your UI when the model changes. 

```
SpMethodListerWithModel >> modelChanged
    list items: aClass selectors sorted
```

```
SpMethodListerWithModel >> defaultLayout
    ^ SpBoxLayout newTopToBottom add: #list ; yourself
```


![A simple list of sorted selectors changing based on its model.](figures/PointThenRectangleSelectors.png label=pointRectangeSelectors&width=100)

Now we can open our widget and as the following script shows it, it will react to change of the model \(see Fig. *@pointRectangeSelectors@*\).

```
| lister |
lister := SpMethodListerWithModel on: Point.
lister open.
lister model: Rectangle
```

Now you know that you can easily build application ui populated from a model and reacting to model changes.



 
### User interface building: a model of UI presentation 
 
A key aspect of Spec is that all user interfaces are constructed through the reuse and composition of existing user interfaces. 
To allow this, defining a user interface consists of defining the _model_ of the user interface, and _not_ the user interface elements that will be shown on screen. 
These UI elements are instantiated by Spec, taking into account the underlying UI framework. 
 
In the end, it is the presentation model and the UI elements that make up the resulting user interface that is shown. This composition of the presentation models is represented as a Presenter object as in Model-View-Presenter. Considering the construction and orchestration of the different widgets in a user interface, Spec is inspired by the Model-View-Presenter pattern. The presenter that is defined in Spec corresponds to a presenter in the MVP triad as shown in Figure *@mvpfig@*.

![A presenter is a model of presentation: It is in relationships with the widgets, its domain model. 
It composes other presenters to form a presenter tree.](figures/MVP.pdf label=mvpfig&width=60)

 
 
To define a new user interface, the developer should create a subclass of `SpPresenter`. 
 
Fundamentally, it is built around three concerns that materialize themselves as the following three methods in `SpPresenter`: 
- the method `initializePresenters` treats the widgets themselves 
- the method `connectPresenters` treats the interactions between widgets 
- the method `defaultLayout` treats the layout of the widgets 
 
 
These methods are hence typically found in the presentation model for each user interface. 
In this chapter we describe the finer points of each method and how these three work together to build the overall UI. 
 
### The _initializePresenters_ method 
@sec_initializeWidgets 
 
The method `initializePresenters` instantiates, saves in instance variables, and partially configures the different widgets that will be part of the UI. 
Instantiation of the presentation models will cause the instantiation and initialization of the different lower-level user interface components, constructing the UI that is shown to the user. 
A first part of the configuration of each widget is specified in _initializePresenters_ as well. 
The focus of this method is to specify what the widgets will look like and what their self-contained behavior is. 
The behavior to update model state, e.g., when pressing a `Save` button, is described in this method as well. 
It is explicitly _not_ the responsibility of this method to define the interactions _between_ the widgets. 
 
In general the `initializePresenters` method should follow the pattern: 
 
- widget instantiation 
- widget configuration specification 
- specification of focus order 
 
 
The last step is not mandatory recommended. 
Indeed, without this final step keyboard navigation may not work reliably. 
 
**Note.** Specifying the method `initializePresenters` is mandatory, as without it the UI would have no widgets. 
 
 
 
 
 
#### Widget instantiation 
 
The instantiation of the model for a widget can be done in two ways: through the use of a creation method or through the use of the `instantiate:` method. 
 
- Considering the first option, the framework provides unary messages for the creation of all basic widgets. The format of these messages is `new[Widget]`, for example `newButton` creates a button widget, and `newList` creates a list widget. The complete list of available widget creation methods can be found in the class `SpPresenter` in the protocol `widgets`. 
 
 
- The second option is more general: to reuse a `SpPresenter` subclass (other than the ones handled by the first option) the widget needs to be instantiated using the `instantiate:` method. For example, to reuse a `MessageBrowser`  widget, the code is `self instantiate: MessageBrowser`. The `instantiate:` method has the responsibility to build an internal parent presenter tree. 
 
 
### Defining UI Layouts 
 
@sec_layoutmethod 
 
Widget layout is defined by specifying methods that state how the different widgets that compose a UI are placed. 
In addition, it also specifies how a widget reacts when the window is resized. 
As we will see later, these methods can have different names. 
 
 
The method `defaultLayout` is an instance method, but it can be also defined at the class level.  
Put differently, typically all the instances of the same user interface have the same layout but a layout 
can be specific to one instance and be dynamic.
 
**Note.** Specifying a layout is mandatory, as without it the UI would show no widgets to the user. 
 
 
#### Having multiple layouts for a widget 
 
For the same UI, multiple layouts can be described, and when the UI is built the use of a specific layout can be indicated. To do this, instead of calling `open` (as we have done until now), use the `openWithLayout:` message with  a layout as argument. For example, consider the following artificial example of a two button UI that has two different layouts: horizontal and vertical.
 
 We define a new presenter named `SpTwoButtons`.
 
``` 
SpPresenter << #SpTwoButtons 
    slots: { #button1 . #button2 }; 
    package: 'CodeOfSpec20BookThreePillar'
```

We define a simple `initializePresenters` method as follows: 

```
SpTwoButtons >> initializePresenters 
    button1 := self newButton. 
    button2 := self newButton. 

    button1 label: '1'.
    button2 label: '2'.
```

We define two class method method returning different layouts.
Note that we could define such methods on the instance side too and we define them on the class side to be able to get such layouts without an instance of the class.

``` 
SpTwoButtons class >> buttonRow 
     
    ^ SpBoxLayout newLeftToRight 
        add: #button1; add: #button2; 
        yourself 
``` 
 
``` 
SpTwoButtons class >> buttonCol 
    ^ SpBoxLayout newTopToBottom 
        add: #button1; add: #button2; 
        yourself 
``` 
 
Note that when we define the layout at the class level, we use a symbol whose name is the corresponding instance variable. Hence we use `#button2` to refer to the presenter stored in the instance variable `button2`. This mapping can be customized at the level of the presenter but we do not present this because we never got the need for it. 


We define a `defaultLayout` merthod just invoking one of the previously defined method.
``` 
SpTwoButtons >> defaultLayout 
    ^ self class buttonRow 
```

We define also a `defaultLayout` method so that the presenter can be opened without defining a given layout. 

##### With openWithLayout:
This UI can be opened in multiple ways: 

- `SpTwoButtons new openWithLayout: SpTwoButtons buttonRow` places the buttons in a row. 
- `SpTwoButtons new openWithLayout:  SpTwoButtons buttonCol` places them in a column. 


Now we can do better and define two instance level methods to encapsulate the layout configuration. 

```
SpTwoButtons >> beCol
    self layout: self class buttonCol
```

```
SpTwoButtons >> beRow
    self layout: self class buttonRow
```

Now we can write the following script

```
SpTwoButtons new 
    beCol;
    open 
```







### Specifying a layout when reusing a presenter 

Having multiple layouts for a presenter implies that there is a way to specify the layout to use when a presenter is reused. This is simple we use the method `layout:`.
Here is an example. 
We create a new presenter named: `SpButtonAndListH`.
 
``` 
SpPresenter << #SpButtonAndListH 
    slots: { #buttons . #list }; 
    package: 'CodeOfSpec20BookThreePillar'
```


```
SpButtonAndListH >> initializePresenters 
    buttons := self instantiate: SpTwoButtons. 
    list := self newList. 
    list items: (1 to: 10). 
``` 

``` 
SpButtonAndListH >> initializeWindow: aWindowPresenter 
    aWindowPresenter title: 'SuperWidget' 
``` 


```
SpButtonAndListH >> defaultLayout 
 
    ^ SpBoxLayout newLeftToRight 
          add: buttons;
          add: list; 
          yourself 
```


This `SpButtonAndListH ` class results in a SuperWidget window as shown in Figure *@fig_alternativeButton@*.  
It reuses the `SpTwoButtons` widget, and places all three widgets in a horizontal order because the `SpTwoButtons` widget will use the `buttonRow` layout method. 
 
![Screen shot of the UI with buttons placed horizontally](figures/alternativeButton.png width=50&label=fig_alternativeButton) 
 
Alternatively, we can create `TBAndListV` class as a subclass of `SpButtonAndListH ` and only change the `defaultLayout` method as below.
It specifies that the reused `buttons` widget should use the `buttonCol` layout method, and hence results in the window shown in
Figure
*@fig_SuperWidget@*.


![Screen shot of the UI with buttons placed vertically](figures/SuperWidget.png width=50&label=fig_SuperWidget) 



``` 
SpButtonAndListH << #TButtonAndListV 
    package: 'CodeOfSpec20BookThreePillar'
``` 
 
```
initializePresenters

    super initializePresenters.
    buttons beCol
```


##### Alternative to declare subcomponent layout choice.

The alternative is to define a new method `defaultLayout` and to use the `add:layout:`. 

We define a different presenter

```
SpButtonAndListH << #TButtonAndListV2
    package: 'CodeOfSpec20BookThreePillar'
```

We define a new `defaultLayout` method as follows: 

``` 
SpButtonAndListV2 >> defaultLayout

    ^ SpBoxLayout new
        add: buttons layout: #buttonCol;
        add: list;
        yourself
```

Note the use of the method `add:layout:` with the selector of the method returning the layout configuration 
here #buttonCol. This is normal since we cannot access state of a subcomponent at this moment.



Note that we can change the layout dynamically from an inspector as shown in *@figTweak@*.

![Tweaking and playing interactively with layouts from the inspector.](figures/Interactive.png width=100&label=figTweak) 





### The _connectPresenters_ method 
 
@sec_initializePresenter 
 
The method `connectPresenters` defines the interactions between the different widgets. 
By connecting the behaviors of the different widgets it specifies the overall presentation, i.e., how the overall UI responds to interactions by the user. 
Usually this method consists of specifications of actions to perform when a certain event is received by a widget. 
The whole interaction flow of the UI then emerges from the propagation of those events. 
 
**Note** The method `connectPresenters` is optional method for a Spec UI. 
 
In Spec, the different UI models are contained in value holders, and the event mechanism relies on the announcements of these value holders to manage the interactions between widgets. 
Value holders provide the method `whenChangedDo:` that is used to register a block to perform on change and the method `whenChangedSend: aSelector to: aReceiver` to send a meessage to a given object. 
In addition to these primitive methods, the basic widgets provide more specific hooks, e.g., when an item in a list is selected (`whenSelectionChangedDo:`). 
 
 
### Conclusion 

In this chapter we have given a more detailed description of how the three fundamental methods of Spec: `initializePresenters`, `defaultLayout` and `connectPresenters` are each responsible for a different aspect of the user interface building process. We also discussed in detail the ability to use different layout methods and how the lookup of layout methods is performed. 

Although reuse is fundamental in Spec, we did not explicitly treat it in this chapter. Instead we refer to the next chapter for more information.