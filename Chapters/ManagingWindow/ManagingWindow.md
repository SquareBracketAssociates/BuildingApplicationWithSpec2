## Managing windows

@cha_managing_windows


So far we have talked about the reuse of `SpPresenter`s, discussed the fundamental functioning of Spec, and presented how to layout the widgets of a user interface. Yet what is still missing for a working user interface is showing all these widgets inside of a window. In our examples until now we have only shown a few of the features of Spec for managing windows, basically restricting ourselves to opening a window.

In this chapter, we provide a more complete overview of how Spec allows for the managing of windows. We show opening and closing, the built-in dialog box facility, the sizing of windows, and all kinds of window decoration.


### A working example

To illustrate the window configuration options that are available, we use a simple `WindowExamplePresenter` class that has two buttons placed side by side. These buttons do not have any behavior associated yet. The behavior will be added in an example further down this chapter.

![A rather simple window on WindowExamplePresenter.](figures/WindowExamplePresenterWindow.png width=50&label=windowExample1)


```
SpPresenter << #WindowExamplePresenter
	slots: { #button1 . #button2 };
	package: 'CodeOfSpec20Book''
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

A user interface can be opened as a normal window or opened as a dialog box, i.e. without decoration and with _Ok_ and _Cancel_ buttons. We will show how this is done, including the configuration options specific to dialog boxes. See also Section *@sec_win_size_decoration@* for more information about window decoration.

### Opening a window

As we have shown in previous chapters, to open a user interface you need to instantiate the `SpPresenter` for that interface and send the `open` message to the instance. That results in the creation of an instance of `SpWindowPresenter` which points to the window containing the user interface, and showing it in a window on the screen.

We have also seen the `openWithLayout:` method that takes a layout (instance of SpLayout subclasses) as an argument. Instead of using the default layout, the opened UI will use the layout passed as an argument.

For example, below we show the two ways we can open a window for our `WindowExamplePresenter`. It will open two identical windows as shown in *@windowExample1@*.

```
| presenter |
presenter := WindowExamplePresenter new.
presenter open.
presenter openWithLayout: presenter defaultLayout
```

### Opening a dialog box


Spec provides an easy way to open a UI as a simple dialog box with _Ok_ and _Cancel_ buttons. A dialog box does not have icons for resizing and closing, nor a window menu. To open a dialog box, send the message `openDialog` as below:

```
| presenter dialog |
presenter := WindowExamplePresenter new.
dialog := presenter openDialog
```


The answer of sending `openDialog`, assigned to the `dialog` variable above, is an instance of the `SpDialogWindowPresenter` class (a subclass of `SpWindowPresenter`).

![A rather simple window on WindowExamplePresent.](figures/WindowExamplePresenterDialog width=50&label=windowDialog)


The `SpDialogWindowPresenter` instance can be configured in multiple ways. To execute code when the user clicks on a button, send it the `okAction:` or `cancelAction:` message with a zero-argument block.

```
| presenter dialog |
presenter := WindowExamplePresenter new.
dialog := presenter openDialog
	okAction: [ Transcript show: 'okAction'; cr ];
	cancelAction: [ Transcript show: 'cancelAction'; cr ];
	whenClosedDo: [ Transcript show: 'whenClosedDo'; cr ]
```

The message `canceled` sent to `dialog` will return `true` if the dialog is closed by clicking on the _Cancel_ button.

### Preventing window close

Spec provides for the possibility to check if a window can effectively be closed when the user clicks on the close box. To use it, this feature must be turned on first by sending `askOkToClose: true` to the `SpWindowPresenter`. This can be done for example by changing our `WindowExamplePresenter` as follows:

```
WindowExamplePresenter >> initializePresenters

   button1 := self newButton.
   button2 := self newButton.
   button1 label: '+'.
   button2 label: '-'.
   self askOkToClose: true
```


The behavior of the close button however is still not changed, closing a window is still possible. This is because we have not defined the implementation of what to check on window close. This is most easily done by overriding the `okToChange` method of `SpPresenter`, for example:

```
WindowExamplePresenter >> requestWindowClose

   ^ false
```


Because this method returns `false`, clicking on the close button of an open `WindowExamplePresenter` window will have no effect. We have effectively created an unclosable window! To be able to close this window, we have to change the implementation of the above method to return `true`, or simply remove it.

Of course, the example `requestWindowClose` method above is extremely simplistic and not very useful. Instead, it should define application-dependent logic of what to check on window close. Note that there are many examples of `okToChange` methods in the system that can be used as inspiration.


### Acting on window close or open

It is also possible to perform an action whenever a window is closed, both with a plain window or a dialog window.

#### With a window

When you want to be notified that a window is closed, you should redefine the `initializeWindow:` method in the class of your presenter as follows:

```
WindowExamplePresenter >> initializeWindow: aWindowPresenter

	aWindowPresenter whenClosedDo: [
		self application newInform title: 'When closed'; openModal ]
```

Then the following snippet programmatically opens and closes a window and you should see the notification triggered on close.

```
| presenter window |
presenter := WindowExamplePresenter new.
window := presenter open.
window close.
```


#### With a dialog window


When you want the same behavior with a dialog window you can either use the mechanism as described previously (i.e. declare your interest in window closing in the method `initializeWindow:`) or configure the dialog presenter returned by the message `openDialog`.

```
| presenter dialog |
presenter := WindowExamplePresenter new.
dialog := presenter openDialog.
dialog
	okAction: [ Transcript show: 'okAction'; cr ];
	cancelAction: [ Transcript show: 'cancelAction'; cr ];
	whenClosedDo: [ dialog application newInform title: 'Bye bye!'; openModal ]
```


#### Action with Window

```
withWindowDo: [ :window | window title: 'MyTitle' ]
```

`withWindowDo:` makes sure that the presenter that scheduled the window still exists or is in a state that makes sense.


### Window size and decoration
@sec_win_size_decoration

Now we focus on sizing a window before and after opening it, and then describe removing the different control widgets that decorate the window.

#### Setting initial size and changing size


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

After a window is opened, it can also be resized by sending the `resize:` message to the window of the UI. For example, we can change our example's `initializePresenters` method so that the window resizes itself depending on which button is clicked.

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


#### Fixed size

The size of a window can be made fixed size, so that the user cannot resize it by dragging the sides or corners as follows:

```
| presenter |
presenter := WindowExamplePresenter new open.
presenter window beUnresizeable
```

#### Removing window decoration


Sometimes it makes sense to have a window without decoration, i.e. without control widgets. Currently, this configuration cannot be performed on the `SpWindowPresenter` of that window, but the underlying widget library may allow it. Below we show how to get the `Morphic` window of our example and instruct it to remove the different control widgets:

```
| presenter |
presenter := WindowExamplePresenter new open.
presenter window
   removeCollapseBox;
   removeExpandBox;
   removeCloseBox;
   removeMenuBox
```


!!note This window is still closable using the halo menus or by calling `close` on the `SpWindowPresenter` instance \(`presenter` in the example above\).



#### Setting and changing the title


By default, the title of a new window is 'Untitled window'. Of course, this can be changed. The first way is to specialize the method `initializeWindow:`
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


#### Setting the icon (DOES NOT WORK)

At the bottom of the main Pharo window, there is a window taskbar, allowing the user to switch between windows by clicking on the buttons that represent the windows. These buttons have an icon that is meant to represent the window's kind. This icon can be configured through Spec, in two different ways.

First, sending the `windowIcon:` message to the `SpWindowPresenter` allows an icon to be set per window. Note that it does not matter if the message is sent before or after the window is opened.

```
| windowPresenter1 windowPresenter2 |
 windowPresenter1 := WindowExamplePresenter new open.
 windowPresenter1 windowIcon: (windowPresenter1 iconNamed: #thumbsDown).

 windowPresenter2 := WindowExamplePresenter new asWindow.
 windowPresenter2 windowIcon: (windowPresenter2 iconNamed: #thumbsUp).
 windowPresenter2 open
```


Second, the icon can be changed by overriding the `windowIcon` message, as below.

```
WindowExamplePresenter >> windowIcon

	^ self iconNamed: #thumbsUp
```


**Note.** Changing the `windowIcon` method will affect all open windows, as the taskbar is periodically refreshed. This refreshing is also why `windowIcon:` can be sent before or after the window has been opened.

#### Setting the about text


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

Sending the message `openDialog` to a presenter will return the dialog window itself so you can easily ask it `isOk`. When `isOk` answers `true`, the dialog is in a state to provide the data it has collected from the user.

Let's look at an example. We will open a dialog to select some colors.

Configuring the UI makes up for the largest part of the code below, but the interesting part is at the end. The canceled state is the default state of a dialog so we have to tell the dialog that it is not canceled. We do that in the `okAction` block, where the dialog receives the message `beOk`.

Then in the `whenClosedDo:` block, we send `isOk` to the dialog. If that message answers `true`, it makes sense to process the selection of colors. For the sake of simplicity of this example, we just inspect the selected colors.

```
selectedColors := Set new.
presenter := SpPresenter new.
colorTable := presenter newTable
	items: (Color red wheel: 10);
	addColumn: (SpCheckBoxTableColumn new
		evaluated: [ :color | selectedColors includes: color ];
		onActivation: [ :color | selectedColors add: color];
		onDeactivation: [ :color | selectedColors remove: color];
		width: 20;
		yourself);
	addColumn: (SpStringTableColumn new
		evaluated: [ :color | '' ];
		displayBackgroundColor: [ :color | color ];
		yourself);
	hideColumnHeaders;
	yourself.
presenter layout: (SpBoxLayout newTopToBottom
	add: colorTable;
	yourself).
dialog := presenter openDialog.
dialog
	title: 'Select colors';
	okAction: [ :dialog | dialog beOk ];
	whenClosedDo: [ dialog isOk ifTrue: [ selectedColors inspect ] ]
```

### Little dialog presenters

Spec supports some little predefined dialogs to inform or request information from the users. Most of them inherit from `SpDialogPresenter`. They offer a builder API to configure them.

The simplest dialog is an alert.

```
SpAlertDialog new
	title: 'Inform example';
	label: 'You are seeing an inform dialog!';
	acceptLabel: 'Close this!';
	openModal
```

Confirm dialogs are created as follows:

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

The idiomatic way to use them is to access them via the application of your presenter class:

```
self application newAlert
	title: 'Inform example';
	label: 'You are seeing an inform dialog!';
	acceptLabel: 'Close this!';
	openModal
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


In this chapter, we treated the features of Spec that have to do with windows. First we described opening and closing windows as well as how to open a window as a dialog box. That was followed by configuring the window's size and its decorating widgets. We ended this chapter with the small yet important details of the window: its title, icon, and about text.