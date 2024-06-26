## Menubar, Toolbar, Status bar and Context Menus

Often application windows have a menubar that includes all commands provided by the application. Application windows may also have a toolbar, with buttons for commands that are used frequently. Some applications only have a toolbar. Apart from supporting a menubar and toolbar, Spec supports a status bar at the bottom of a window. Some widgets are equipped with context menus, such as a text fields, tables, and lists. All these aspects are the subject of this chapter.

We will build a small email reader application. Apart from adding a menubar, a toolbar, a status bar, and a context menu, the example brings together a lot of what we have seen in the previos chapters. Figure *@mailreader@* shows the target application.

![The mail reader application. % width=60&label=mailreader](figures/mailreader.png)

The example is elaborate, with a lot of classes and methods. We will implement the application bottom-up. We start with the models. Afterwards, we will implement the presenters that compose the application. Finally, we will add a menubar, a toolbar, a status bar, and a context menu. Let's dive in.


### The models

To build the mail reader, we need three models:

* `Email` represents an email.
* `MailFolder` represents a folder that holds emails, like "Inbox", "Draft", and "Sent".
* `MailAccount` represents a mail account. It holds all the emails.

#### Email

In Figure *@mailreader@*, we see that the application shows four fields for an email. "From" holds sender. "To" holds the addressee. "Subject" holds the subject of an email. The nameless text field at the bottom-right holds the body of an email. Let's define an `Email` class to cover these fields.

```
Object << #Email
	slots: { #from . #to . #subject . #body . #status };
	package: 'CodeOfSpec20Book'
```

We will not include the accessors for `from`, `to`, `subject`, and `status` here. They are trivial.

Note that there is a fifth instance variable called `status`. This instance variable will be used to keep track of the status of an email, either "received", "draft", or "sent". These statuses map onto the mail folders in the application, respectively "Inbox", "Draft", and "Sent". We define the following methods to change the status of an email. They will come in handy when we receive, create, or send emails.

```
Email >> beReceived

	status := #received
```

```
Email >> beDraft

	status := #draft
```

```
Email >> beSent

	status := #sent
```

To know what the status of an email is, we define three more messages.

```
Email >> isReceived

	^ status = #received
```

```
Email >> isDraft

	^ status = #draft
```

```
Email >> isSent

	^ status = #sent
```

We will not define accessors for the `status` instance variable. The six methods above keep the status nicely encapsulated.

Now we can define the `initialize` method. It states that a new email is in draft status by default.

```
Email >> initialize

	super initialize.
	self beDraft
```

We define two final methods. They are related to including emails in a tree presenter. The first method answers the string that should be displayed in the list of emails.

```
Email >> displayName

	^ subject
```

The second method answers what should be displayed as children in a tree presenter. While a folder has children, i.e. its emails, an email does not have any children, so this method returns an empty array. We do not use tree-related terminology, as it would not be appropriate. Therefore we use `content`, as in "the content of a folder".

```
Email >> content

	^ Array new
```

#### MailFolder

The list on the right side of the window does not only displays emails. It also displays mail folders, which group emails according to their state. We will define the `MailFolder` model very simplisticly. It has a name and it holds emails.

```
Object << #MailFolder
	slots: { #emails . #name };
	package: 'CodeOfSpec20Book'
```

At initialization time, a `MailFolder` does not have any emails, and its name is `nil`.

```
MailFolder >> initialize

	super initialize.
	emails := OrderedCollection new
```

That defines the default state of a `MailFolder` instance, but an instance creation method is handy:

```
MailFolder class >> named: aString emails: aCollection

	^ self new
		name: aString;
		emails: aCollection;
		yourself
```

This method needs these accessor methods:

```
MailFolder >> emails: aCollection

	emails := aCollection
```

```
MailFolder >> name: aString

	name := aString
```

Similarly to the `Email` class, we need some tree-related methods:

```
MailFolder >> displayName

	^ name
```

```
MailFolder >> content

	^ emails
```

From this implementation, you can see that a `MailFolder` is just a named container object for emails, which can be used to structure the display of emails in a tree presenter.


#### MailAccount

A `MailAccount` holds all emails, so the definition of the class is simple:

```
Model << #MailAccount
	slots: { #emails };
	package: 'CodeOfSpec20Book'
```

Note that this is the first email reader model class that inherits from `Model`. To keep things simple, the email reader application will depend only on a `MailAccount` instance, not on `Email` and `MailFolder` instances.

Initialization is trivial:

```
MailAccount >> initialize

	super initialize.
	emails := OrderedCollection new
```

We know that emails have a status and that the status is used to split emails in separate folders. That is where the following methods come in:

```
MailAccount >> receivedEmails

	^ emails select: [ :each | each isReceived ]
```

```
MailAccount >> draftEmails

	^ emails select: [ :each | each isDraft ]
```

```
MailAccount >> sentEmails

	^ emails select: [ :each | each isSent ]
```

Given that `MailAccount` is the main model of the application, it defines some actions.

First of all, emails can be fetched. In a real application, emails come from a server. We do not want to go that far. Therefore, we put one email in the account.

```
MailAccount >> fetchMail

	| email |
	email := Email new
		from: 'book@pharo.org';
		to: 'readers@pharo.org';
		subject: 'The Spec 2.0 book has been released';
		body: 'Dear reader,
The Spec 2.0 book is available.
Best regards.';
		beReceived;
		yourself.
	(emails includes: email) ifFalse: [ emails add: email ].
	self changed
```

This method creates a new email, and gives it the "received" status. Then it adds the email to the emails it already holds. Adding is done conditionallt because we do not want the same email appearing twice after fetching multiple times.

Note `self changed` at the end. It notifies dependents that a `MailAccount` instance changed in a general way. Again, we like to keep things simple. More specific change messages are possible, but we do not need them in this example application.

The user of the application can create new emails and save them. When they are saved, they are draft emails, as this method defines:

```
MailAccount >> saveAsDraft: anEmail

	anEmail beDraft.
	(emails includes: anEmail) ifFalse: [ emails add: anEmail ].
	self changed
```

Saving a method as draft is implemented as changing the status to "draft" and adding it to the emails, if it is not present yet. The conditional adding allows saving an email multiple times without adding it multiple times.

The method to send an email is similar to the method to save an email:

```
MailAccount >> send: anEmail

	anEmail beSent.
	(emails includes: anEmail) ifFalse: [ emails add: anEmail ].
	self changed
```

Finally, an email can be deleted. The implementation is simple. Remove the email from the account and let dependent know.

```
MailAccount >> delete: anEmail

	emails remove: anEmail.
	self changed
```

That concludes our models. Now we can dig into the presenters.

### The presenters

TODO