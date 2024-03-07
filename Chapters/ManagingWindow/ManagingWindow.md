## Managing windows

@cha_managing_windows

status: need a pass before review need some explanation about modal window.
status: spellchecked

So far we have talked about the reuse of `SpPresenter`s, discussed the fundamental functioning of Spec, and presented how to layout the widgets of a user interface. Yet what is still missing for a working user interface is showing all these widgets inside of a window. In our examples until now we have only shown a few of the features of Spec for managing windows, basically restricting ourselves to opening a window.

In this chapter, we provide a more complete overview of how Spec allows for the managing of windows. We show opening and closing, the built-in dialog box facility, the sizing of windows, and all kinds of window decoration.


### A working example

To illustrate the window configuration options that are available, we use a simple `WindowExample` class that has two buttons placed side by side. These buttons do not have any behavior associated yet, this will be added in an example further down this chapter.

![A rather simple window on WindowExamplePresenter.](figures/WindowExamplePresenterOpen1.png width=40&label=windowExample1)


```
SpPresenter << #WindowExamplePresenter
   slots: {#button1 . #button2};
   package: 'SpecBook'
```

```
WindowExamplePresenter >> initializePresenters
   button1 := self newButton.
   button2 := self newButton.
   button1 label: '+'.
   button2 label: '-'
```

```
WindowExamplePresenter >> defaultLayout
   ^ SpBoxLayout newLeftToRight
         add: #button1;
         add: #button2;
         yourself
```

### Opening a window or a dialog box

A user interface can be opened as a normal window or opened as a dialog box, i.e. without decoration and with _Ok_ and _Cancel_ buttons. We show here how this is done, including the configuration options specific to dialog boxes. See also Section *@sec_win_size_decoration@* for more information about window decoration.

### Opening a window

As we have shown in previous chapters, to open a user interface you need to instantiate the `SpPresenter` for that interface and send it the `open` message. This creates an instance of `SpWindowPresenter` which points to the window containing the user interface and shows it in a window on the screen.

We have also seen the `openWithLayout:` method that takes a layout (instance of SpLayout subclasses) as an argument. 
Instead of using the default layout, the opened UI will use the layout passed as an argument. 

For example, below we show the two ways we can open a window for our `WindowExamplePresenter`. It will open two identical windows as shown in *@windowExample1@*.

```
| presenter |
presenter := WindowExamplePresenter new.
presenter open.
presenter openWithLayout: aLayout
```

### Opening a dialog box and its configuration options


Spec provides an easy way to open a UI as a simple dialog box with _Ok_ and _Cancel_ buttons \(that has no icons for resizing, closing, or the window menu\). To do this, send the message `openDialog` as below:

```
| presenter dialog |
presenter := WindowExamplePresenter new.
dialog := presenter openDialog
```


The result of this (e.g. assigned to the `dialog` variable above) is an instance of the  `SpDialogWindowPresenter` class (a subclass of `SpWindowPresenter`).

![A rather simple window on WindowExamplePresent.](figures/WindowExamplePresenterDialog width=40&label=windowDialog)


The `SpDialogWindowPresenter` instance can also be configured in multiple ways. To execute code when the user clicks on a button, send it the `okAction:` or `cancelAction:` message with a zero-argument block.

```
| presenter dialog |
presenter := WindowExamplePresenter new.
dialog := presenter openDialog
        okAction: [ Transcript show: 'okAction' ];
        cancelAction: [ Transcript show: 'cancelAction' ];
        whenClosedDo: [ Transcript show: 'whenClosedDo' ]
```


The message `canceled` sent to `dialog` will return `true` if the dialog is closed by clicking on the _Cancel_ button.

### Preventing window close

Spec provides for the possibility to check if a window can effectively be closed when the user clicks on the close box. To use it, this feature must first be turned on, by sending `askOkToClose: true` to the `SpWindowPresenter`. This can be done for example by changing our `WindowExamplePresenter` as follows:

```
WindowExamplePresenter >> initializePresenters
   button1 := self newButton.
   button2 := self newButton.
   button1 label: '+'.
   button2 label: '-'.
   self askOkToClose: true
```


The behavior of the close button however is still not changed, closing a window is still possible. This is because we have not defined the implementation of what to check on window close. This is most easily done by overriding the `okToChange` method of `SpPresenter`, for example as below:

```
WindowExample >> okToChange
   ^ false
```


Because this method returns `false`, clicking on the close button of an open `WindowExample` window will not have any effect. We have effectively created an unclosable window! To be able to close this window, we should change the implementation of the above method to return `true`, or simply remove it.

Of course, the example `okToChange` method above is extremely simplistic and not very useful. Instead, it should define application-dependent logic of what to check on window close. Note that there are many examples of `okToChange` methods in the system that can be used as inspiration.


### Acting on window close or open

It is also possible to perform an action whenever a window is closed, both with a plain window or a dialog window.

#### With a window

When you want to get notified that a window is closed, you should redefine the `initializeWindow:` method in the class of your presenter as follows:

```
WindowExamplePresenter >> initializeWindow: aWindowPresenter

    aWindowPresenter whenClosedDo: [
			self newInform title: 'When closed'; openModal ]
```



Then the following snippet programmatically opens and closes a window and you should see the notification triggered on close.
```
| we window |
we := WindowExamplePresenter3 new.
window := we open.
window close.
```


#### With a dialog window


When you want the same behavior with a dialog window you can either use the mechanism as described previously (i.e. declare your interest in window closing in the method `initializeWindow:`) or configure the dialog presenter returned by the message `openDialog`.

```
| presenter dialog |
presenter := WindowExamplePresenter new.
dialog := presenter openDialog.
dialog
    okAction: [ Transcript show: 'okAction' ];
    cancelAction: [ Transcript show: 'cancelAction' ].
    whenClosedDo: [ self newInform title: 'Bye bye!'; openModal ]
```


#### Action with Window

```
withWindowDo: [ :window | window title: 'MyTitle' ]
```

`withWindowDo:` makes sure that the presenter that scheduled the window still exists or is in a state that makes sense.


### Window size and decoration
@sec_win_size_decoration

We now focus on sizing a window before and after opening it, and then talk about removing the different control widgets that decorate the window.

### Setting initial size and changing size


To set the initial size of a window when it opens, send the `initialExtent:` message to the corresponding `SpWindowPresenter` before opening, for example like this:

```
| windowPresenter |
 windowPresenter := WindowExamplePresenter new asWindow.
 windowPresenter initialExtent: 300@80.
 windowPresenter open
```

The common way to specify the initial size of the window is to use the message `initialExtent:` as follows:

```
WindowExamplePresenter >> initializeWindow: aWindowPresenter

  aWindowPresenter initialExtent: 80@100
```

Note that you can also set an initial position using the message `initialPosition:`.

After a window is opened, it can also be resized by sending the `resize:` message to the window of the UI. For example, we can change our example's `initializePresenters` method so that the window resizes itself depending on what button is clicked.

```
WindowExamplePresenter >> initializePresenters

  button1 := self newButton.
  button2 := self newButton.
  button1 label: '+'.
  button2 label: '-'.
  button1 action: [ self window resize: 500@200].
  button2 action: [ self window resize: 200@100]
```

You have also `centered`, `centeredRelativeTo:` and `centeredRelativeToTopWindow` to help you place the windows relative to world/other windows.


### Fixed size

The size of a window can be made fixed size, so that the user cannot resize it by dragging the sides or corners as follows:

```
| windowPresenter |
windowPresenter := WindowExample new open.
windowPresenter window beUnresizeable
```

### Removing window decoration


Sometimes it makes sense to have a window without decoration, i.e. without control widgets. Currently, this configuration cannot be performed on the `SpWindowPresenter` of that window, but the underlying widget library may allow it. Below we show how to get the `Morphic` window of our example and instruct it to remove the different control widgets:

```
| windowPresenter |
windowPresenter := WindowExamplePresenter new open.
windowPresenter window
   removeCollapseBox;
   removeExpandBox;
   removeCloseBox;
   removeMenuBox
```


!!note This window is still closable using the halo menus or by calling `close` on the `SpWindowPresenter` instance \(`windowPresenter` in the example above\).



### Setting and changing the title


By default, the title of a new window is _'Untitled window'_. Of course, this can be changed. The first way is to specialize the method `initializeWindow:`
to send the message `title:` to the `windowPresenter` as follows:

```
WindowExamplePresenter >> initializeWindow: aWindowPresenter
       aWindowPresenter title: 'Click to grow or shrink.'
```


In addition, you can set the title of any UI after it has been opened (even if it specifies a `title` method) by sending the `title:` message with the new title as an argument to the window of the UI. An example is:

```
| presenter |
presenter := WindowExamplePresenter new.
presenter open.
presenter window title: 'I am different!'
```


### Setting the icon


!!todo Does not work :\( 


At the bottom of the main Pharo window, there is a window taskbar, allowing the user to switch between windows by clicking on the buttons that represent each window. These buttons also have an icon that is meant to represent the window's kind. This icon can also be configured through Spec, in two different ways.

Firstly, sending the `windowIcon:` message to the `SpWindowPresenter` allows an icon to be set per window, as below. Note that it does not matter if the message is sent before or after the window is opened.

```
| windowPresenter1 windowPresenter2 |
 windowPresenter1 := WindowExamplePresenter new open.
 windowPresenter1 windowIcon: (Smalltalk ui icons iconNamed: #thumbsDown).

 windowPresenter2 := WindowExample new asWindow.
 windowPresenter2 windowIcon: (Smalltalk ui icons iconNamed: #thumbsUp).
 windowPresenter2 open
```


Secondly, the icon can be changed by overriding the `windowIcon` message, as below.

```
WindowExamplePresenter >> windowIcon
   ^ self iconNamed: #thumbsUp
```


**NOTE:** Changing the `windowIcon` method will affect all open windows, as the taskbar is periodically refreshed. This refreshing is also why `windowIcon:` can be sent before or after the window has been opened.

### Setting the about text


To set the about text of a window, either override the `aboutText` method of the corresponding `SpPresenter` so that it returns the new about text, or sends the instance the `aboutText:` message before opening, for example as below.

```
| windowPresenter |
 windowPresenter := WindowExamplePresenter new asWindow.
 windowPresenter aboutText: 'Click + to grow, - to shrink.'.
 windowPresenter open
```

### Modal windows

A modal window is a window that takes control of the entire Pharo user interface, making it impossible for the user to select another window while it is open. This is especially useful for dialog boxes, but may also be needed for other kinds of windows.

### Getting values from a dialog window

By default the `openModal` sent to a dialog window will return the dialog window itself
so you can easily ask it `isOk`.



### Little dialog presenters

Spec supports some little predefined dialogs to inform or request information from the users.
Most of them inherit from `SpDialogPresenter`. They offer a builder API to configure them.

The simplest dialog is an alert.

```
SpAlertDialog new
	title: 'Inform example';
	label: 'You are seeing an inform dialog!';
	acceptLabel: 'Close this!';
	openModal
```

Confirm dialog are created as follows:

```
SpConfirmDialog new
	title: 'Confirm example';
	label: 'Are you sure?';
	acceptLabel: 'Sure!';
	cancelLabel: 'No, forget it';
	onAccept: [ :dialog| dialog alert: 'Yes!' ];
	onCancel: [ :dialog| dialog alert: 'No!' ];
	openModal
```


The following example is not working because
- `openModal` does not return the dialog
- second the dialog does not offer `isOk`

```
| ok |
ok := SpConfirmDialog new.
ok
	title: 'Confirm modal example';
	label: 'Are you sure?';
	acceptLabel: 'Sure!';
	cancelLabel: 'No, forget it';
	openModal.

ok application newAlert (ok
	ifTrue: [ 'Yes!' ]
	ifFalse: [ 'No!' ])
```


So we can do it with

```
| app ok inform dialog |
app := SpApplication new.
confirm := app newConfirm.
confirm
	title: 'Confirm modal example';
	label: 'Are you sure?';
	acceptLabel: 'Sure!';
	cancelLabel: 'No, forget it'.

dialog := confirm asModalWindow.
dialog title: (dialog isOk
	ifTrue: [ 'Yes!' ]
	ifFalse: [ 'No!' ]).
dialog open.
```

The idiomatic way to use them is to access them via the application or presenter doing

```
	...
	self application newAlert
	...
	self newAlert
	....
```

`SpApplication` offers the following API: `newConfirm`, `newAlert`, `newJobList`, `newRequest`, `newSelect`, `newRequestText`.


### Placing a presenter inside a dialog window

Any presenter can be placed in a dialog window by specializing the method `initializeDialogWindow:`.
Here is a simple example showing how the default buttons are set. 



```
initializeDialogWindow: aDialogWindowPresenter
	"Used to initialize the model in the case of the use into a dialog window.
	 Override this to set buttons other than the default (Ok, Cancel)."

	aDialogWindowPresenter
		addButton: 'Ok' do: [ :presenter |
			self accept.
			presenter close ];
		addButton: 'Cancel' do: [ :presenter |
			presenter close ]
```
You can also define  in your presenter how it will behave when it is open in a dialog window

### Conclusion


In this chapter, we treated the features of Spec that have to do with windows. We first talked about opening and closing windows as well as how to open a window as a dialog box. This was followed by configuring the window size and its decorating widgets. We ended this chapter with the small yet important details of the window: its title, icon, and about text.