## Commander: A powerful and simple command framework
@cha_commander


Commander was a library originally developed by Denis Kudriashov. Commander 2.0 is the second iteration of that library. It was designed and developed by Julien Delplanque and Stéphane Ducasse. Note that Commander 2.0 is not compatible with Commander but it is really easy to migrate from Commander to Commander 2.0. We describe Commander 2.0 in the context of Spec. From now on, when we mention Commander we refer to Commander 2.0. In addition, we show how to extend Commander to other needs.



### Commands


Commander models application actions as first-class objects following the Command design pattern. With Commander, you can express commands and use them to generate menus and toolbars, but also to script applications from the command line.

Every action is implemented as a separate command class \(subclass of `CmCommand`\) with an `execute` method and the state required for execution.

![A simple command and its hierarchy.](figures/BasicCommand.pdf width=35&label=first)

We will show later that for a UI framework, we need more information such as an icon and shortcut description. In addition, we will present how commands can be decorated with extra functionality in an extensible way.

### Defining commands

A command is a simple object instance of a subclass of the class `CmCommand`. It has a description, a name \(this name can be either static or dynamic as we will show later on\). In addition, it has a context from which it extracts information to execute itself. In its basic form, there is no more than that.

Let us have a look at examples. We will define some commands for the ContactBook application and illustrate how they can be turned into menus and a menubar.


### Adding some convenience methods


For convenience reasons, we define a common superclass of all the commands of the contact book application named `ContactBookCommand`.

```
CmCommand << #ContactBookCommand
    package: 'ContactBook'
```



We define a simple helper method to make the code more readable
```
ContactBookCommand >> contactBookPresenter

    ^ self context
```


For the same reason, we define another helper to access the contact book and the selected item.
```
ContactBookCommand >> contactBook

    ^ self contactBookPresenter contactBook
```


```
ContactBookCommand >> selectedContact

    ^ self contactBookPresenter selectedContact
```


Using the helper method `isContactSelected` we defined in the previous chapter, the method `hasSelectedContact` can be implemented as:

```
ContactBookCommand >> hasSelectedContact

    ^ self contactBookPresenter isContactSelected
```


#### Adding the Add Contact command


We define a subclass to define the add a contact command.

```
ContactBookCommand << #AddContactCommand
    package: 'ContactBook'
```


```
AddContactCommand >> initialize
    super initialize.
    self
        basicName: 'New contact';
        basicDescription: 'Creates a new contact and adds it to the contact book.'
```


```
AddContactCommand >> execute

    | contact |
    contact := self contactBookPresenter newContact.
    self hasSelectedContact
        ifTrue: [ self contactBook addContact: contact after: self selectedContact ]
        ifFalse: [ self contactBook addContact: contact ].
    self contactBookPresenter updateView
```


We define the method `updateView` to refresh the contents of the table.

```
ContactBookPresenter >> updateView
    table items: contactBook contacts
```


Now in an inspector on an instance of `ContactBookPresenter`, we can simply execute the command as follows:

```
(AddContactCommand new context: self) execute
```


Executing the command should ask you to give a name and a phone number and the new contact will be added to the list.

We can also execute the following snippet.

```
| presenter command |
presenter := ContactBookPresenter on: ContactBook coworkers.
command := AddContactCommand new context: presenter.
command execute
```


### Adding the Remove Contact command


Now we define now another command to remove a contact. This example is interesting because it does not involve any UI interaction. It shows that a command is not necessarily linked to UI interaction.

```
ContactBookCommand << #RemoveContactCommand
    package: 'ContactBook'
```


```
RemoveContactCommand >> initialize
    super initialize.
    self
        name: 'Remove';
        description: 'Removes the selected contact from the contact book.'
```


This command definition illustrates how we can control when a command should or should not be executed. The method `canBeExecuted` allows specifying such a condition.

```
RemoveContactCommand >> canBeExecuted
    ^ self context isContactSelected
```


The method `execute` is straightforward.

```
RemoveContactCommand >> execute
    self contactBook removeContact: self selectedContact.
    self contactBookPresenter updateView
```


The following test validates the correct execution of the command.

```
ContactCommandTest >> testRemoveContact

    self assert: presenter contactBook size equals: 3.
    presenter table selectIndex: 1.
    (RemoveContactCommand new context: presenter) execute.
    self assert: presenter contactBook size equals: 2
```



### Turning commands into menu items

Now that we have our commands, we would like to reuse them and turn them into menus. In Spec, commands that are transformed into menu items are structured into a tree of command instances. The class method `buildCommandsGroupWith:forRoot:` of `SpPresenter` is a hook to let presenters define the root of the command instance tree.

A command is transformed into a command for Spec using the message `forSpec`.We will show later that we can add UI-specific information to a command such as an icon and a shortcut.

The method `buildCommandsGroupWith:forRoot:` registers commands to which the presenter instance is passed as context. Note that here we just add plain commands, but we can also create groups. Later in this chapter we will also specify a menu bar in this method.

```
ContactBookPresenter class >>
    buildCommandsGroupWith: presenter
    forRoot: rootCommandGroup

    rootCommandGroup
        register: (AddContactCommand forSpec context: presenter);
        register: (RemoveContactCommand forSpec context: presenter)
```


Now we have to attach the root of the command tree to the table. This is what we do with the new line in the `initializePresenters` method. Notice that we have full control and as we will show we could select a subpart of the tree \(using the message `/`\) and define it as root for a given component.


```
ContactBookPresenter >> initializePresenters
    table := self newTable.
    table
        addColumn: (SpStringTableColumn title: 'Name' evaluated: #name);
        addColumn: (SpStringTableColumn title: 'Phone' evaluated: #phone).
    table actions: self rootCommandsGroup.
    table items: contactBook contacts.
```


When reopening the interface with `(ContactBookPresenter on: ContactBook coworkers) open`, you should see the menu items as shown in Figure *@withmenu@*. As we will show later, we could even replace a menu item with another one, changing its name, or icon in place.

![With two menu items with groups.](figures/withMenus.png width=60&label=withmenu)

### Introducing groups


Commands can be managed in groups and such groups can be turned into corresponding menu item sections. The key hook method is the class method named `buildCommandsGroupWith: presenterInstance forRoot:`.


Here we give an example of such grouping. Note that the message `asSpecGroup` is sent to a group. We create two methods, each creating a simple group, one for adding, and one for removing contracts.

```
ContactBookPresenter class >> buildAddingGroupWith: presenter

    ^ (CmCommandGroup named: 'Adding') asSpecGroup
        description: 'Commands related to contact addition.';
        register: (AddContactCommand forSpec context: presenter);
        beDisplayedAsGroup;
        yourself
```


```
ContactBookPresenter class >> buildRemovingGroupWith: presenter

    ^ (CmCommandGroup named: 'Removing') asSpecGroup
        description: 'Commands related to contact removal.';
        register: (RemoveContactCommand forSpec context: presenter);
        beDisplayedAsGroup;
        yourself
```


We group the previously defined groups together under the contextual menu:

```
ContactBookPresenter class >> buildContextualMenuGroupWith: presenter

    ^ (CmCommandGroup named: 'Context Menu') asSpecGroup
        register: (self buildAddingGroupWith: presenter);
        register: (self buildRemovingGroupWith: presenter);
        yourself
```


Finally, we revisit the hook `buildCommandsGroupWith:forRoot:` to register the last group to the root command group.

```
ContactBookPresenter class >>
    buildCommandsGroupWith: presenter
    forRoot: rootCommandGroup

    rootCommandGroup
        register: (self buildContextualMenuGroupWith: presenter)
```


When reopening the interface with `(ContactBookPresenter on: ContactBook coworkers) open`, you should see the menu items inside a `'Context Menu'` as shown in Figure *@withmenuContext@*.

![With a context menu.](figures/withmenuContext.png width=60&label=withmenuContext)

To show that we can also select part of the command tree, we select the `'Context Menu'` group and declare it as the root of the table menu. Then you will not see the `'Context Menu'` anymore.

```
ContactBookPresenter >> initializePresenters

    table := self newTable.
    table
        addColumn: (SpStringTableColumn title: 'Name' evaluated: #name);
        addColumn: (SpStringTableColumn title: 'Phone' evaluated: #phone).
    table actions: self rootCommandsGroup / 'Context Menu'.
    table items: contactBook contacts
```

Here we see that by sending the slash message \(` / `\), we can select the group in which we want to add a menu iten.


### Extending menus


Building menus is nice, but sometimes we need to add a menu to an existing one. Commander supports this via a dedicated pragma, called `<extensionCommands>` that identifies extensions.


Imagine that we have new functionality that we want to add to the contact book and that this behavior is packaged in another package, here, `ContactBook-Extensions`.
First, we will define a new command and second, we will show how we can extend the existing menu to add a new menu item. 

```
ContactBookCommand << #ChangePhoneCommand
    package: 'ContactBook-Extensions'
```


```
ChangePhoneCommand >> initialize

    super initialize.
    self
        name: 'Change phone';
        description: 'Change the phone number of the contact.'
```


```
ChangePhoneCommand >> execute

    self selectedContact phone: self contactBookPresenter newPhone.
    self contactBookPresenter updateView
```


We extend `ContactBookPresenter` with the method `newPhone` to let the presenter decide how a user should provide a new phone number.

```
ContactBookPresenter >> newPhone

    | phone |
    phone := self
        request: 'New phone for the contact'
        initialAnswer: self selectedContact phone
        title: 'Set new phone for contact'.
    (phone matchesRegex: '\d\d\d\s\d\d\d')
        ifFalse: [
            SpInvalidUserInput signal: 'The phone number is not well formatted.
Should match "\d\d\d\s\d\d\d"' ].
    ^ phone
```


The last missing piece is the declaration of the extension. This is done using the pragma `<extensionCommands>` on the class side of the presenter class as follows:

```
ContactBookPresenter class >>
    changePhoneCommandWith: presenter
    forRootGroup: aRootCommandsGroup

    <extensionCommands>

    (aRootCommandsGroup / 'Context Menu')
        register: (ChangePhoneCommand forSpec context: presenter)
```



![With menu extension.](figures/withmenuExtension.png width=60&label=withmenuExtension)


### Managing icons and shortcuts


By default a command does not know about Spec-specific behavior, because a command does not have to be linked to UI. Obviously you want to have icons and shortcut bindings when you are designing an interactive application.

Commander supports the addition of icons and shortcut keys to commands.Let us see how it works from a user perspective. The framework offers two methods to set an icon and a shortcut key: `iconName:` and `shortcutKey:`.
We should specialize the method `asSpecCommand` as follows:

```
RemoveContactCommand >> asSpecCommand

    ^ super asSpecCommand
        iconName: #removeIcon;
        shortcutKey: $x meta;
        yourself
```



```
AddContactCommand >> asSpecCommand

    ^ super asSpecCommand
        shortcutKey: $n meta;
        iconName: #changeAdd;
        yourself
```


Note that the commands are created using the message `forSpec`. This message takes care of the calling `asSpecCommand`.

### Enabling shortcuts


At the time of writing this chapter, Commander management of shortcuts has not been pushed to Spec to avoid dependency on Commander. It is then the responsibility of your presenter to manage shortcuts as shown in the following method.
We ask the command group to install the shortcut handler in the window.

```
ContactBookPresenter >> initializeWindow: aWindowPresenter

    super initializeWindow: aWindowPresenter.
    self rootCommandsGroup installShortcutsIn: aWindowPresenter
```


### In-place customisation


Commander supports the reuse and in-place customisation of commands. It means that a command can be modified on the spot: for example, its name or description can be adapted to the exact usage context. Here is an example that shows that we adapt the same command twice.

Let us define a really simple and generic command that will simply inspect the object.

```
ContactBookCommand << #InspectCommand
    package: 'ContactBook-Extensions'
```


```
InspectCommand >> initialize

    super initialize.
    self
        name: 'Inspect';
        description: 'Inspect the context of this command.'
```


```
InspectCommand >> execute

    self context inspect
```


By using a block, the context is computed at the moment the command is executed and the name and description can be adapted for its specific usage as shown in Figure *@inPlaceCustomisation@*.

```
ContactBookPresenter class >>
    extraCommandsWith: presenter
    forRootGroup: aRootCommandsGroup

    <extensionCommands>

    aRootCommandsGroup / 'Context Menu'
        register:
            ((CmCommandGroup named: 'Extra') asSpecGroup
                description: 'Extra commands to help during development.';
        register:
            ((InspectCommand forSpec context: [ presenter selectedContact ])
                name: 'Inspect contact';
                description: 'Open an inspector on the selected contact.';
                iconName: #smallFind;
                yourself);
        register:
            ((InspectCommand forSpec context: [ presenter contactBook ])
                name: 'Inspect contact book';
                description: 'Open an inspector on the contact book.';
                yourself);
        yourself)
```


![With menu extension.](figures/inPlaceCustomisation.png width=95&label=inPlaceCustomisation)

### Managing a menu bar


Commander also supports menu bar creation.
The logic is the same as for contextual menus: we define a group and register it under a given root, and we tell the presenter to use this group as a menubar.

Imagine that we have a new command to print the contact.

```
ContactBookCommand << #PrintContactCommand
    package: 'ContactBook'
```


```
PrintContactCommand >> initialize

    super initialize.
    self
        name: 'Print';
        description: 'Print the contact book in Transcript.'
```


```
PrintContactCommand >> execute

    Transcript open.
    self contactBook contacts do: [ :contact | self traceCr: contact name , ' - ' , contact name ]
```



We create a simple group that we call 'MenuBar' \(but it could be called anything\).

```
ContactBookPresenter class >> buildMenuBarGroupWith: presenter

    ^ (CmCommandGroup named: 'MenuBar') asSpecGroup
        register: (PrintContactCommand forSpec context: presenter);
        yourself
```


We modify the root to add the menu bar group in addition to the previou one.
```
ContactBookPresenter class >>
    buildCommandsGroupWith: presenter
    forRoot: rootCommandGroup

    rootCommandGroup
        register: (self buildMenuBarGroupWith: presenter);
        register: (self buildContextualMenuGroupWith: presenter)
```


We hook it into the widget as the last line of the `initializePresenters` method. Notice the use of the message `asMenuBarPresenter` and the addition of a new instance variable called `menuBar`.

```
ContactBookPresenter >> initializePresenters

    table := self newTable.
    table
        addColumn: (SpStringTableColumn title: 'Name' evaluated: #name);
        addColumn: (SpStringTableColumn title: 'Phone' evaluated: #phone).
    table actions: self rootCommandsGroup / 'Context Menu'.
    table items: contactBook contents.
    menuBar := (self rootCommandsGroup / 'MenuBar') asMenuBarPresenter.
```


Finally, to get the menu bar we declare it in the layout. We use `SpAbstractPresenter class>>#toolbarHeight` to specify the height of the menu bar.

```
ContactBookPresenter >> defaultLayout

    ^ SpBoxLayout newVertical
            add: #menuBar
            withConstraints: [ :constraints | constraints height: self toolbarHeight ];
            add: #table;
            yourself
```


![With a menubar.](figures/withmenubar.png width=60&label=inPlaceCustomisation2)


### Conclusion

In this chapter, we saw how you can define a simple command and execute it in a given context. We show how you can turn a command into a menu item in Spec by sending the message `forSpec`. You learned how we can reuse and customize commands. We presented groups of commands as a way to structure menus and menu bars.
