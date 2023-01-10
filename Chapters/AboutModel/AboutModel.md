## About presenter on a model

status: ready for review

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


### Conclusion

Now you know that you can easily build application ui populated from a model and reacting to model changes.



