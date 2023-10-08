## Dynamic Presenters
@cha_dynamic_presenter

Contrary to Spec 1, in Spec 20 all the layouts are dynamic. It means that you can change on the fly the elements displayed. It is a radical improvement from Spec1 where most of the layouts were static and building dynamic widgets was cumbersome.

In this chapter, we will show that presenters can be dynamically composed using layouts. We will show a little interactive section. Then we will build a little code editor with dynamic aspects. Note that In this post, we are going to use simply Spec, to refer to Spec 20 when we do not need to stress a difference.

### Layouts as simple as objects

Building dynamic applications using Spec is simple. In fact, any layout in Spec is dynamic and composable. For example, let me show you the following code snippet:

```
"Instantiate a new presenter"
presenter := SpPresenter new.
"Optionally, define an application for the presenter"
presenter application: SpApplication new.
```

There are three principal layouts in Spec: `SpPanedLayout`, `SpBoxLayout`, and `SpGridLayout`. For this presenter, we will use the `SpPanedLayout` which can receive two presenters \(or layouts\) and place them in one half of the window.
If you want to see all the available layouts on Spec, you can check the package `Spec2-Layout`.

```
presenter layout: SpPanedLayout newTopToBottom.
presenter open.
```


Of course, we are going to see an empty window because we did not put anything in the layout as shown in Figure *@layout1@*.

![An empty layout.](figures/layout1.png width=60&label=layout1)


Now, without closing the window, we can dynamically edit the layout of the main presenter. We will add a button presenter executing the following lines:

```
presenter layout add: (button1 := presenter newButton).
button1 label: 'I am a button'.
```


![Paned layout with one button.](figures/layout2.png width=60&label=layout2)


Now, we can add another button. There is no need to close and reopen the window, everything updates dynamically and without the need of rebuilding the window. As we instantiate the layout with `newTopToBottom`, the presenters will align vertically.

```
presenter layout add: (button2 := presenter newButton).
button2 label: 'I am another button'.
```


![Paned layout with two buttons.](figures/layout3.png width=60&label=layout3)

Now, we can put an icon for the first button:

```
button1 icon: (button1 iconNamed: #smallDoIt).
```


![Paned layout with two buttons.](figures/layout4.png width=60&label=layout4)

Or we can delete one of the buttons from the layout \(as shown in Figure *@layout5@*\)

```
presenter layout remove: button2.
```


![Removing a button.](figures/layout5.png width=60&label=layout5)

!!note What we should see here is that all the changes happen simply by creating a new instance of a given layout and sending messages to it.  It means that programs can create simply complex logic of the dynamic behavior of a widget.

### Creating a presenter that dynamically adds buttons with random numbers

We will create a presenter in which we will add and remove dynamically buttons. We will create a new class called `DynamicButtonsPresenter`.

![A presenter that dynamically adds buttons](figures/layout8.png width=60&label=layout8)

```
SpPresenter << #DynamicButtonsPresenter
	slots: { #addPresenterButton . #removePreseterButton . #textPresenter . #canRemovePresenter };
	package: 'DynamicButtons'
```

Now, we need to initialize the presenters. For doing so, we need to override the method `initializePresenters`. We want to have a button that when we click on it, it adds a new button into the layout.

```
addPresenterButton := self newButton.
	addPresenterButton
		action: [ self addToLayout ];
		label: 'Add a presenter to the layout';
		icon: (self iconNamed: #smallAdd).
```

We also want a button that will remove the last button that was added, if any, of the layout.

```
removePreseterButton := self newButton.
	removePreseterButton
		action: [ self removeFromLayout ];
		label: 'Remove a presenter from the layout';
		icon: (self iconNamed: #smallDelete);
		disable.
```

And finally, let's add a text presenter that will cannot be removed.

```
textPresenter := self newText.
	textPresenter
		text: 'I am a text presenter.
		I will not be removed';
		beNotEditable
```

Here is the full code for the `initializePresenters` method.

```
initializePresenters

	addPresenterButton := self newButton.
	addPresenterButton
		action: [ self addToLayout ];
		label: 'Add a presenter to the layout';
		icon: (self iconNamed: #smallAdd).
		
	removePreseterButton := self newButton.
	removePreseterButton
		action: [ self removeFromLayout ];
		label: 'Remove a presenter from the layout';
		icon: (self iconNamed: #smallDelete);
		disable.
		
	textPresenter := self newText.
	textPresenter
		text: 'I am a text presenter.
		I will not be removed';
		beNotEditable
```

Now, we need to implement the methods `addToLayout` and `removeFromLayout`. Those methods, as they name indicate, add and remove presenters dynamically from the layout.

Let's start with the `addToLayout` method. We will enable the remove presenter button. Because as we are adding a new presenter, that means that we can remove it.

```
addToLayout

    | randomButtonName newButton |
	removePreseterButton enable.

	randomButtonName := 'Random number: ', (Random new nextInteger: 1000) asString.

	newButton := self newButton
		label: randomButtonName;
		icon: (self iconNamed: #smallObjects);
        yourself

	self layout add: newButton expand: false
```

For removing a button of the layout, we will first check if there is a button that we can remove. If true, we will just remove the last button. Then, if there is no more buttons left to remove, we will disable the remove button.

```
removeFromLayout

	self layout remove: self layout presenters last.

	self layout presenters last = textPresenter
		ifTrue: [ removePreseterButton disable ]
```

The last thing that is missing to implement is to specify the default layout. For that, we need to override the method `defaultLayout`.

```
defaultLayout

	^ SpBoxLayout newTopToBottom
		add: addPresenterButton expand: false;
		add: removePreseterButton expand: false;
		add: textPresenter;
		yourself		
```

![Adding random buttons](figures/layout9.png width=60&label=layout9)

### Building a little dynamic browser


Now, with all of this knowledge, we are going to build a new mini version of the System Browser as shown in Figure *@layout6@*. We want to have:

- A tree that shows all the system classes.
- A list that shows all methods in the selected class.
- A text presenter that shows the code of a selected method and a button.


Initially, the code of the method will be in “Read-Only” mode. When we press the button, we are going to pass to “Edit” mode.


![A little code browser.](figures/layout6.png width=60&label=layout6)



Let us get started. So, first, we need to create a subclass of `SpPresenter`, called `MyMiniBrowserPresenter`.

```
SpPresenter < #MyMiniBrowserPresenter
    slots: {#treeClasses . #button . #codeShower . #methodsList};
    package: 'MiniBrowser'
```

    
Now, we need to override the `initializePresenters` method in which we are going to 
initialize the presenters and the layout of our mini browser.

First, we are going to instantiate the tree presenter.
We want the tree presenter to show all the classes that are presented in the Pharo image. 
We know that all subclasses (almost) inherit from `Object`. 
So, that is going to be the only root of the tree. 
To get the subclasses of a class we can send the message subclasses, 
that is what we need to get the children of a node. 
We want each of the nodes (clases) to have a nice icon, we can get the icon of a class with the message `systemIcon`. 
Finally, we want to “activate” the presenter with only one click instead of two. 

The code is:

```
MyMiniBrowserPresenter >> initializePresenters
 
    treeClasses := self newTree.
    treeClasses
       activateOnSingleClick;
       roots: { Object };
       children: [ :each | each subclasses ];
       displayIcon: [ :each | each systemIcon ]
```


For the methods, we want to have a filtering list. That means, a list in which we can search of elements. Also, we want that to display only the selector of the method to the user and sort them in an ascending way.

```
methodsFilteringList := self newFilteringList.
methodsFilteringList display: [ :method | method selector ].
methodsFilteringList listPresenter
    sortingBlock: [ :method | method selector ] ascending.
```

    
We said that, initially, the code is going to be in “Read-Only” mode. 
So, the label of the button is going to be “Edit” to say that if we click on the button we will change to edition mode. Also we want to have a nice icon.

```
button := self newButton.
button
   label: 'Edit';
   icon: (self iconNamed: #smallConfiguration).
```


As the initial behavior will be read-only mode, the code shower will be only a text presenter that is not editable.

```
codeShower := self newText.
codeShower beNotEditable.
```

Here the complete code of the method is:

```
MyMiniBrowserPresenter >> initializePresenters
 
    treeClasses := self newTree.
    treeClasses
        activateOnSingleClick;
        roots: Object asOrderedCollection;
        children: [ :each | each subclasses ];
        displayIcon: [ :each | each systemIcon ].
 
    methodsFilteringList := self newFilteringList.
    methodsFilteringList display: [ :method | method selector ].
    methodsFilteringList listPresenter
        sortingBlock: [ :method | method selector ] ascending.
 
    button := self newButton.
    button
        label: 'Edit';
        icon: (self iconNamed: #smallConfiguration).
 
    codeShower := self newText.
    codeShower beNotEditable
```


### Placing elements visually

We initialized our presenter but we did not indicate how they need to be displayed. For doing so, we need to override the method `defaultLayout` and return a layout.

We want the upper part of the layout to have the classes and the methods shown in a horizontal way, like in the System Browser \(a.k.a. Calypso\). 
So, what we will do is to create another left-to-right layout, with a spacing of 10 pixels, the classes, and the methods.

Then, we will add that layout to our main layout. the main layout is going to be a top-to-bottom layout. After, we want the code shower and then the button. 
We do not want the code to expand. In addition, we want a separation of 5 pixels for this layout.

```
MyMiniBrowserPresenter >> defaultLayout
 
    | classesAndMethodsLayout |
    classesAndMethodsLayout := SpBoxLayout newLeftToRight.
    classesAndMethodsLayout
        spacing: 10;
        add: treeClasses;
        add: methodsFilteringList.
    ^ SpBoxLayout newTopToBottom
        spacing: 5;
        add: classesAndMethodsLayout;
        add: codeShower;
        add: button expand: false;
        yourself
```


So far, so good… but we did not add any behavior to the presenters. To do that we can either do it in the `initializePresenters` method or override the `connectPresenters` method. To clearly separate the intention of the methods, we favor overriding `connectPresenters`.

### Connecting the flow

When we click on a class of the tree, we want to update the items of the methods list with the methods of the selected class. When we click on a method, we should update the text of the code shower with the source code of the method.

```
MyMiniBrowserPresenter >> connectPresenters
 
    treeClasses whenActivatedDo: [ :selection | 
        methodsFilteringList items: selection selectedItem methods ].
     
    methodsFilteringList listPresenter
        whenSelectedDo: [ :selectedMethod | 
            codeShower text: selectedMethod ast formattedCode ].
     
    button action: [ self buttonAction ]
```


For now, we define the method `buttonAction` to do nothing.

```
MyMiniBrowserPresenter >> buttonAction
```



### Toggling Edit/Readonly mode

When we click on the button we want several things. 
That is why it is better to create a separate method. 

- First, we want to change to label to the button to alternate between “Edit” and “Read-Only”. 
- Then, we want to change the presenter of the code shower. If the Mini Browser is on read-only mode we want to have a text presenter that is not editable. And if the Mini Browser is on edit mode we want to have a code presenter that highlights the code and shows the number of lines of code. But always the code shower is going to have the same text \(the code of the methods\).


```
MyMiniBrowserPresenter >> buttonAction
 
   | newShower |
	button label = 'Edit'
		ifTrue: [
			button label: 'Read only'.
			newShower := self newCode
				beForMethod: methodsFilteringList selectedItem;
				text: methodsFilteringList selectedItem ast formattedCode;
				yourself ]
		ifFalse: [
			button label: 'Edit'.
			newShower := self newText
				text: methodsFilteringList selectedItem ast formattedCode;
				beNotEditable;
				yourself ].
	
	self layout replace: codeShower with: newShower.
	codeShower := newShower
```



As a last detail, because we love details, we do not want the “Untitled window” as the window title and also we want a default extent. We override `initializeWindow:` method.

```
MyMiniBrowserPresenter >> initializeWindow: aWindowPresenter
 
    aWindowPresenter
        title: 'My Mini Browser';
        initialExtent: 750 @ 650
```


Voilà! We have a new version minimal version of the System Browser with a read-only mode. If we run `MyMiniBrowserPresenter new open` we should get *@layout7@*.

![Our little code browser in readonly mode.](figures/layout7.png width=60&label=layout7)

### Conclusion

With Spec we can build from simple applications to very sophisticated ones. 
The dynamic layouts are simply nice. 
Layouts can be configured in multiple ways, so have a look at their classes and the examples available.
Spec has lots of presenters that are ready to be used. 
Start digging into the code to see which presenters are available, what are their API.
