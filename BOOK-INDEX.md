# Book index: Application Building with Spec 2.0

A static map of this book for readers and coding agents: which chapter solves which problem,
which Spec API each section uses, where the matching runnable examples live in a Pharo image,
and which book code no longer matches the current API.

- Validated against: Pharo 15 image (P15-SpecToploAnotherTry), 2026-11-06.
  Every name under `API:` was checked to have an implementor in a `Spec2-*`, `Commander2*`
  or `NewTools-Inspector*` package; every `Sp…`/`Cm…` class name was checked to exist.
- Chapter order follows `index.md`. Chapters marked *(not in the printed book)* are commented
  out or not included there.
- Classes the book defines itself (`ImdbFilm…`, `Mail…`, `Todo…`, …) are not in the image.
  Their code is at https://github.com/SquareBracketAssociates/CodeOfSpec20Book.
- The book is backend-neutral: UI code uses presenters, layouts, transmissions, commands and
  styles only. A backend (Morphic, GTK, Toplo) is only chosen through the application configuration.

## How to use this file

- Find a problem: search the `Solves:` lines.
- Find an API: search the `API:` lines, or the reverse index at the end
  (one line per class or selector).
- Read only what you need: open the chapter file and jump to the section heading in quotes.
  Headings are exact `###` titles, so `grep -n '^### Heading' file` gives the line.
- Run an example: the `Image examples:` line names classes or `Class class>>selector` methods
  present in the image. In the image, `SpDemo open` opens the demo browser.
- Before copying book code: read the `Stale:` line of the chapter.

Field meanings:
- `Solves:` the problems the chapter answers.
- `Prerequisites:` chapters to read first.
- `Defines:` classes created by the book code (not in the image).
- `Sections:` each line is `"Heading" — classes | selectors`.
- `Image examples:` runnable examples on the same topic in the image.
- `Stale:` book code that does not match the validated image.
  `none` means everything checked is fine.

---

## Introduction — Chapters/Intro/Intro.md
Solves: what Spec is, why presenters are reusable, the multiple-backend idea, where the code is.
Prerequisites: none
Sections:
- "Reuse of logic" — SpPresenter
- "Spec 2.0" — differences with Spec 1.0
- "Code" — how to load the book code
- "Acknowledgements"
Image examples: `SpDemo` (open with `SpDemo open`)
Stale: the text names GTK and Morphic as the backends; the image also has Toplo. No API.

## A 10 min small example — Chapters/FirstContact/FirstContact.md
Solves: first complete UI; create a presenter class, add subpresenters, define a layout,
set the window title and size, open and close it.
Prerequisites: none
Defines: CustomerSatisfactionPresenter
Sections:
- "A customer satisfaction UI" — goal
- "Create the class of the UI" — SpPresenter
- "Instantiate and configure subpresenters" — SpButtonPresenter SpBoxLayout SpTPresenterBuilder SpIconProvider | newButton newLabel label: icon: iconNamed: action: newLeftToRight newTopToBottom instantiate:
- "Define a title and window size, open and close the UI" — SpWindowPresenter | initializeWindow: title: initialExtent: open close
- "Conclusion"
Image examples: `SpButtonPresenter class>>example`, `SpLabelPresenter class>>example`, `SpDemoButtonsPresenter`
Stale: none

## Most of Spec in one example — Chapters/CaseStudyOne/CaseStudyOne.md
Solves: a whole small application (films) touching every core concept: application,
table, grid form, modal dialog, reusing a presenter, connecting presenters, UI tests,
changing a layout, transmissions, styles.
Prerequisites: FirstContact
Defines: ImdbApp ImdbFilm ImdbFilmListPresenter ImdbFilmPresenter ImdbConfiguration
Sections:
- "Application" — SpApplication
- "A basic film model"
- "List of films" — SpTablePresenter SpStringTableColumn SpBoxLayout | newTable addColumn: title:evaluated:
- "Filling up the film list" — items:
- "Opening presenters via the application" — newPresenter:
- "Improving the window" — newToolbar newToolbarButton toolbar: icon: iconNamed: title: initialExtent:
- "An application manages icons"
- "FilmPresenter" — SpGridLayout | build: add:at: nextRow newTextInput newNumberInput rangeMinimum:maximum:
- "Better looking FilmPresenter" — SpGridLayout | beColumnNotHomogeneous column:expand:
- "Opening FilmPresenter in a modal dialog" — openModal
- "Customizing the modal dialog" — addButton:do: beOk centered
- "Invoking a presenter" — newApplication: isOk updatePresenter
- "Embedding a FilmPresenter into the FilmListPresenter" — instantiate: editable:
- "Define component communication" — SpPresenterWithModel | whenSelectionChangedDo: selectedItem setModel:
- "Testing your application UI" — SUnit setUp/tearDown on presenters
- "Adding more tests" — isMultipleSelection listSize
- "Changing layout" — openWithLayout: layout:
- "Using transmissions" — SpModelPort | transmitTo: inputModelPort
- "Styling the application" — SpMorphicConfiguration | configure: addStyleSheetFromString: addStyle: useBackend:with: column:withConstraints:
- "Conclusion"
Image examples: `SpDemoFormPresenter`, `SpDemoStandaloneFormPresenter`, `SpTablePresenter class>>exampleWithColumnHeaders`, `SpTransmissionExample`
Stale: none

## Spec core in a nutshell — Chapters/InANutshell/InANutshell.md
Solves: the architecture: presenters, application, application configuration and backends,
layouts, styles, navigation between presenters.
Prerequisites: none
Defines: MyMainPresenter ImdbMorphicConfiguration ImdbGTKConfiguration
Sections:
- "Spec architecture overview"
- "Spec core architecture overview"
- "Presenters" — SpPresenter SpPresenterWithModel SpButtonPresenter
- "Application" — SpApplication | newApplication:
- "Application configuration" — SpMorphicConfiguration SpGTKConfiguration | configure: useBackend:with: styleSheet
- "Layouts" — SpBoxLayout SpPanedLayout SpTabLayout | newLeftToRight newTopToBottom
- "Styles and stylesheets" — SpStyle SpStyleVariableSTONReader | defaultStyleSheet addStyle:
- "Navigation between presenters" — whenSelectionChangedDo: whenTextChangedDo: whenModelChangedDo: whenSubmitDo:
- "Conclusion"
Image examples: `SpApplication class>>example1`, `SpApplication class>>example2`, `SpDemo`
Stale: "Application configuration": the GTK backend is not loaded in the validated image
(`SpGTKConfiguration`, `installTheme:`, `addCSSProviderFromString:` missing). Morphic code is fine.

## Testing Spec applications — Chapters/TestingInSpec/TestingInSpec.md
Solves: testing presenters and interaction logic without a backend (clicks, selections,
opened windows), and testing an application.
Prerequisites: FirstContact, ThreePillarsOfSpec
Defines: ColorChooser ColorChooserApplication
Sections:
- "Testing presenters" — SpAbstractBackendForTest
- "Spec user example" — SpPresenter | newList newText newButton displayBackgroundColor: whenSelectionChangedDo:
- "Tests" — click clickAtIndex: isBuilt delete
- "Testing your application" — SpApplication | windows closeAllWindows start
- "Known limitations and conclusion"
Image examples: none in `Spec2-Examples`; see the `Spec2-Tests` package.
Stale: none

## The dual aspects of presenters: Domain and interaction model — Chapters/ThreePillarsOfSpec/ThreePillarsOfSpec.md
Solves: when to use `SpPresenter` or `SpPresenterWithModel`; the three hooks
`initializePresenters`, `connectPresenters`, `defaultLayout`.
Prerequisites: FirstContact
Defines: MethodLister MethodListerWithModel
Sections:
- "About presenters on a model" — SpPresenter SpPresenterWithModel
- "Example with SpPresenter" — on: newList items:
- "SpPresenter vs. SpPresenterWithModel" — setModel: on:
- "Example with SpPresenterWithModel" — newApplication:model: model model:
- "User interface building: a model of UI presentation"
- "The _initializePresenters_ method" (anchor sec_initializeWidgets)
- "The _connectPresenters_ method" (anchor sec_connectPresenter)
- "The _defaultLayout_ method" (anchor sec_layoutmethod)
- "Conclusion"
Image examples: `SpDemoStandaloneFormPresenter`, `SpDemoFormPresenter` (both `SpPresenterWithModel`)
Stale: none

## Reuse and composition at work — Chapters/Reuse/Reuse.md
Solves: build a presenter meant to be reused as a widget, combine presenters, give the
reused presenter a public API, choose or change the layout of a reused presenter.
Prerequisites: FirstContact, ThreePillarsOfSpec
Defines: WidgetClassListPresenter ProtocolMethodListPresenter ProtocolViewerPresenter
Sections:
- "First requirements" — SpAbstractWidgetPresenter SpListPresenter SpTextPresenter SpLabelPresenter
- "Creating a basic UI to be reused as a widget" (anchor sec_WidgetClassList) — newList items: focusOrder
- "Supporting reuse" — whenSelectionChangedDo:
- "Combining two basic presenters into a reusable UI" (anchor sec_protocollist) — display: unselectAll selection
- "Live inspection of the widgets"
- "Writing tests"
- "Managing three widgets and their interactions" (anchor sec_protocolviewer) — instantiate: resetSelection
- "Having different layouts" — openWithLayout:
- "Enhancing our API"
- "Changing the layout of a reused widget" (anchor sec_protocolbrowser) — SpCodePresenter | beForMethod: syntaxHighlight:
- "Changing layouts" — layout:
- "Considerations about a public configuration API" (anchor sec_public_API)
- "New versus old patterns" — connectPresenters
- "Conclusion"
Image examples: `SpClassMethodBrowser`, `SpExampleBrowser`, `SpPresenterSelectorExample`
Stale: none

## Lists, tables and trees — Chapters/ListTreeTable/ListTreeTable.md
Solves: show collections: lists (display, decoration, single or multiple selection,
drag and drop, filtering), component lists, trees, tables (headers, sorting, editing), tree tables.
Prerequisites: FirstContact
Sections:
- "Lists" — SpListPresenter | items:
- "Controlling item display" — display: sortingBlock:
- "Decorating elements" — displayIcon: displayColor: displayBold: displayItalic: displayUnderline:
- "About single/multiple selection" — SpSingleSelectionMode SpMultipleSelectionMode SpTreeSingleSelectionMode SpTreeMultipleSelectionMode | beMultipleSelection selectedItem whenSelectionChangedDo:
- "Drag and drop" — dragEnabled: dropEnabled: wantsDrop: acceptDrop: passenger
- "Activation clicks"
- "Filtering lists" — SpFilteringListPresenter | applyFilter: topLayout
- "Selectable filtering lists" — SpFilteringSelectableListPresenter
- "Component lists" — SpComponentListPresenter | presenters:
- "Trees" — SpTreePresenter SpMenuPresenter | roots: children: expandPath: selectPathByItems:scrollToSelection: contextMenu:
- "Tables" — SpStringTableColumn SpCheckBoxTableColumn SpImageTableColumn SpIndexTableColumn SpDropListTableColumn SpCompositeTableColumn
- "First table" — SpTablePresenter | addColumn: title:evaluated: showColumnHeaders
- "Sorting headers" — sortFunction: compareFunction:
- "Editable tables" — beEditable onAcceptEdition: beSortable
- "Tree tables" — SpTreeTablePresenter | beExpandable beResizable
- "Conclusion"
Image examples: `SpListPresenter class>>example…` (8), `SpTablePresenter class>>example…` (14),
`SpTreePresenter class>>example…` (7), `SpTreeTablePresenter class>>example…` (8),
`SpFilteringListPresenter class>>example…` (3), `SpComponentListPresenter class>>example`,
`SpDemoListsPresenter`, `SpDemoTablePresenter`, `SpDemoTreeTablePresenter`, `SpDemoFilteringListsPresenter`, `SpListSelectionPresenter`
Stale: none. Not covered by the book: `SpListViewPresenter` and `SpTreeListViewPresenter`
(package `Spec2-ListView`, 42 class-side examples).

## Managing windows — Chapters/ManagingWindow/ManagingWindow.md
Solves: open a presenter as a window or a dialog, prevent or react to closing, size and
decorate a window, get values back from a dialog, predefined alert/confirm dialogs, keyboard
focus, act when a window opens.
Prerequisites: FirstContact, LayoutContruction
Defines: WindowExamplePresenter
Sections:
- "A working example"
- "Opening a window or a dialog box" — SpWindowPresenter SpDialogWindowPresenter | openWithLayout: openDialog okAction: cancelAction:
- "Preventing window close" — SpWindowWillClose | whenWillCloseDo: denyClose
- "Acting on window close" — whenClosedDo: withWindowDo:
- "Window size and decoration" (anchor sec_win_size_decoration) — asWindow window initialExtent: resize: aboutText:
- "Getting values from a dialog window" — isOk beOk
- "Little modal dialog presenters" — SpAlertDialog SpConfirmDialog | newAlert acceptLabel: cancelLabel: onAccept: onCancel: openModal
- "Placing a presenter inside a dialog window" — addButton:do: addDefaultButton:do: triggerOkAction triggerCancelAction
- "Setting keyboard focus" — focusOrder
- "Acting on window opening" — initializeDialogWindow: whenOpenedDo: defaultButton takeKeyboardFocus
- "Conclusion"
Image examples: `SpAlertDialog class>>example…`, `SpConfirmDialog class>>example…`,
`SpRequestDialog class>>example…` (including `examplePassword`), `SpOptionListExample class>>exampleDialog`,
`SpInitializeWindowExample`, `SpDemoModalPresenter`
Stale:
- "Little modal dialog presenters": `SpDialogPresenter` does not exist; the dialogs inherit
  from `SpAbstractMessageDialog` and `SpStandardDialogPresenter`.
- "Opening a window or a dialog box": "instance of SpLayout subclasses": the root layout
  class is `SpExecutableLayout`.

## Layouts — Chapters/LayoutContruction/Layout.md
Solves: place subpresenters: box layouts (expand, fill, padding, spacing, alignment), reuse
a presenter with another layout, change a layout at run time, grid, paned and overlay layouts.
Prerequisites: FirstContact
Defines: TwoButtons ButtonAndListH ButtonAndListV ButtonAndListV2 AlignmentExample PanedLayoutExample OverlayLayoutExample
Sections:
- "Basic principle reminder" — SpBoxLayout SpPanedLayout
- "A running example"
- "BoxLayout (SpBoxLayout and SpBoxConstraints)" — SpBoxConstraints | add:expand: add:expand:fill:padding: add:withConstraints: spacing: padding: height:
- "Box layout alignment"
- "Box alignment example" — hAlignStart hAlignCenter hAlignEnd vAlignStart vAlignCenter vAlignEnd
- "Alignment in horizontal box layout"
- "A more advanced layout"
- "Example setup for layout reuse"
- "Opening with a layout"
- "Better design"
- "Specifying a layout when reusing a presenter" — instantiate:
- "Alternative to declare subcomponent layout choice" — add:layout:
- "Dynamically changing a layout"
- "Grid layout (SpGridLayout)" — SpGridLayout SpGridLayoutBuilder | build: add:at: add:at:span: add:span: nextRow nextColumn
- "Paned layout (SpPanedLayout)" — positionOfSlider:
- "Overlay layout (SpOverlayLayout)" — SpOverlayLayout | addOverlay:withConstraints: child:
- "Conclusion"
Image examples: `SpBoxLayout class>>example…` (13: expand, homogeneous, borders, alignments, composite),
`SpPanedLayout class>>exampleNotResizingStartChild`, `SpBoxLayoutAlignmentExample`,
`SpBoxLayoutReplacePresenterExample`, `SpScrollableLayoutExamplePresenter`;
grid: `SpDemoTextInputPresenter`, `SpDemoStandaloneFormPresenter`, `SpDemoButtonsPresenter`;
overlay: `SpSearchInputFieldPresenter class>>example3`
Stale: none

## Dynamic presenters — Chapters/DynamicPresenter/DynamicPresenter.md
Solves: add, remove or replace presenters at run time; build a small dynamic browser;
switch a text between edit and read-only.
Prerequisites: LayoutContruction
Defines: DynamicButtons MyMiniBrowser
Sections:
- "Layouts as simple as objects" — SpPanedLayout | layout layout: remove:
- "Dynamic button adder"
- "Defining add/remove buttons" — presenters remove: enable disable
- "Building a little dynamic browser" — newTree newFilteringList roots: children: activateOnSingleClick
- "Placing elements visually"
- "Connecting the flow" — whenSelectedDo: whenActivatedDo:
- "Toggling Edit/Read-only mode" — newCode beForMethod: beNotEditable replace:with:
- "About layout recalculation"
- "Conclusion"
Image examples: `SpDynamicLayoutExamplePresenter`, `SpDynamicWidgetChange`,
`SpBoxLayoutReplacePresenterExample`, `SpDynamicSpecExample`
Stale: none

## A Concrete Case: A Mail Application — Chapters/MailApp/MailApp.md
Solves: a larger application built bottom-up: domain models, then presenters on those models,
an empty-state presenter, a tree of folders, a paned client window.
Later chapters reuse it (Menus, Style, Commander2).
Prerequisites: ThreePillarsOfSpec, Reuse, LayoutContruction
Defines: Email MailFolder MailAccount EmailPresenter NoEmailPresenter MailReaderPresenter MailAccountPresenter MailClientPresenter
Sections:
- "The models"
- "Email"
- "MailFolder"
- "MailAccount"
- "The presenters"
- "The `EmailPresenter`" — SpPresenterWithModel | model: whenTextChangedDo: editable:
- "The `NoEmailPresenter`" — hAlignCenter vAlignCenter
- "The `MailReaderPresenter`" — layout:
- "The `MailAccountPresenter`" — newTree roots: children: expandRoots selectedPath
- "The `MailClientPresenter`" — SpPanedLayout | positionOfSlider:
- "First full application"
- "Conclusion"
Image examples: none
Stale: none

## Menubar, Toolbar, Status Bar, and Context Menus — Chapters/Menus/Menus.md
Solves: add a menubar, keyboard shortcuts, a toolbar, enabling and disabling items, a status
bar, and context menus, to the mail application, with plain menu presenters (no commands).
Prerequisites: MailApp
Sections:
- "Adding a menubar to a window" — SpMenuBarPresenter SpWindowPresenter | newMenuBar addItem: subMenu: menu:
- "Implementing message menu commands" — newMenu addGroup: shortcut: enabled:
- "Installing shortcuts" — SpMenuPresenter | key bindings on the window (see Stale)
- "Defining actions"
- "Adding a toolbar to a window" — newToolbar newToolbarButton addItem: addItemRight: help: toolbar:
- "Supporting enablement" — enabled:
- "Adding a status bar to a window" — SpStatusBarPresenter | newStatusBar statusBar: pushMessage: popMessage
- "Adding a context menu to a presenter" — newMenu contextMenu:
- "Enabling blocks"
- "Conclusion"
Image examples: `SpMenuBarPresenter class>>example`, `SpToolbarPresenter class>>example`,
`SpStatusBarPresenter class>>example`, `SpMenuPresenter class>>exampleWithGroups`,
`SpMenuPresenter class>>exampleShowAtPointer`, `SpTreePresenter class>>exampleWithContextMenu`,
`SpInitializeWindowExample`, `SpDemoToolbarPresenter`
Stale:
- "Installing shortcuts" (also used again in "Adding a toolbar to a window" and "Adding a status bar
  to a window"): `menuBar addKeybindingsTo: aWindowPresenter` no longer exists. Use
  `addKeyBindingsTo:` (capital B, `SpMenuPresenter`).
- Not covered by the book: the actions API (`addActionWith:`, `addShortcutWith:`, `actions:`,
  `SpAction`), which declares a context menu and shortcuts together. Examples:
  `SpCodePresenter class>>exampleOverridingContextMenu`, `SpListViewPresenter class>>exampleWithActions`.

## Using transmissions and ports — Chapters/Transmissions/Transmission.md
Solves: connect presenters by data flow instead of explicit event handlers: transmit,
transform the transmitted value, act without an input port, act after the transmission,
ports of nested presenters.
Prerequisites: ThreePillarsOfSpec, Reuse
Defines: OverviewDetailPresenter
Sections:
- "What are transmissions?"
- "A simple example" — SpListPresenter SpTextPresenter | whenSelectedItemChangedDo:
- "Basic transmission" — transmitTo: defaultOutputPort defaultInputPort
- "Transforming a transmitted object" — transmitTo:transform:
- "Acting on a transmission without input port" — transmitDo:
- "Acting after a transmission" — transmitTo:transform:postTransmission:
- "Available ports" — SpAbstractPort SpOutputPort SpInputPort SpSelectionPort SpTextPort
- "Ports and nesting presenters"
- "A more advanced example" — SpModelPort SpTreePresenter | transmitTo:postTransmission:
- "Another variation"
- "Conclusion"
Image examples: `SpTransmissionExample`, `SpTransmissionComponentListExample`,
`SpDemoSelectablePresenter`, `SpClassMethodBrowser`, `SpPresenterSelectorExample`
Stale: none

## Styling applications — Chapters/Style/Style.md
Solves: change the look with stylesheets: style declaration and anatomy, environment
variables, application-level stylesheet, applying and removing styles at run time.
Prerequisites: MailApp, InANutshell
Defines: MailClientApplication
Sections:
- "In a nutshell" — SpStyleVariableSTONReader | fromString: addStyle: styleSheet:
- "How do styles work?"
- "Stylesheets"
- "Style declaration"
- "Stylesheet examples" — SpStyle | defaultStyleSheet
- "Anatomy of a style" — SpPropertyStyle SpContainerStyle SpDrawStyle SpFontStyle SpGeometryStyle SpTextStyle
- "Environment variables" — SpStyleEnvironmentVariable
- "Top-level changes"
- "Defining an application and its style" — SpApplication | styleSheet styleSheet: start
- "Applying styles" — addStyle:
- "Dynamically applying styles" — removeStyle:
- "Conclusion"
Image examples: `SpTransmissionComponentListExample` (application stylesheet), `SpTransmissionComponentItemExample`
Stale: none found; the text targets the Morphic stylesheets of Pharo 12.

## Using Athens and Roassal in Spec — Chapters/AthensRoassal/AthensRoassal.md
Solves: custom drawing in a presenter (Athens vector canvas), visualizations (Roassal),
embedding an existing morph.
Prerequisites: FirstContact
Defines: AthensHello AthensHelloPresenter
Sections:
- "Introduction"
- "Direct integration of Athens with Spec" — SpAthensPresenter | drawBlock: surfaceExtent:
- "Roassal Spec integration" — SpRoassalInspectorPresenter
- "SpRoassalPresenter" — SpRoassalPresenter SpColorPicker
- "Hello world in Athens via Morphic objects"
- "Handling resizing"
- "Using the morph with Spec" — SpMorphPresenter | morph:
- "Conclusion"
Image examples: `SpAthensPresenter class>>example`, `SpAthensPresenter class>>exampleResizing`, `SpMorphPresenter class>>example`
Stale: none. Note that `SpMorphPresenter` ties a UI to the Morphic backend.

## Customizing your Inspector — Chapters/Inspector/Inspector.md
Solves: add inspector tabs for your objects (text, table, Roassal chart), activate them
conditionally, remove the evaluator.
Prerequisites: ListTreeTable
Sections:
- "Creating custom tabs" — pragma `<inspectorPresentationOrder:title:>`
- "Adding a tab with text" — SpTextPresenter | beNotEditable
- "A tab with a table" — SpTablePresenter SpStringTableColumn
- "Tab activation condition" — `…Context:` methods, active:
- "Adding a raw view of a specific element of the collection"
- "Removing the evaluator" — withoutEvaluator
- "Adding Roassal charts" — SpRoassalPresenter | asPresenter
- "Conclusion"
Image examples: the methods with the pragma `inspectorPresentationOrder:title:`
(`Pragma allNamed: #inspectorPresentationOrder:title:`)
Stale: none

## Commander: A powerful and simple command framework — Chapters/Commander2/Commander.md
Solves: model actions as command objects (Commander 2) and generate context menus, a menubar
and a toolbar from command groups; icons, shortcuts, groups, extending another tool's menus.
Prerequisites: MailApp, Menus
Defines: MailClientCommand NewMailCommand SaveMailCommand SendMailCommand DeleteMailCommand FetchMailCommand
NewMailTemplateCommand FormatPlainTextCommand FormatRichTextCommand ShowCcFieldCommand ShowBccFieldCommand
Sections:
- "Commands" — CmCommand
- "Defining commands"
- "Adding a common superclass for the command classes" — context
- "Adding the main commands" — name: description:
- "Adding placeholder commands"
- "Turning commands into menu items" — CmCommandGroup | forSpec named: register: beRoot asSpecGroup context:
- "Using fillWith:" — newMenu fillWith: rootCommandsGroup /
- "Managing icons and shortcuts" — asSpecCommand iconName: shortcutKey:
- "Managing a menubar" — newMenuBar buildMenuBarGroupWith:
- "Introducing groups" — beDisplayedAsGroup
- "Extending menus"
- "Declaring extension"
- "Managing a toolbar" — newToolbar buildToolBarGroupWith: beDisplayedOnRightSide
- "Conclusion"
Image examples: `SpCommandGroupExample`, `SpExampleNewCommand`
Stale: none. `Chapters/Commander2/leftout.md` is a draft, not a chapter.

---

## A Todo application in 15min — Chapters/CaseStudyTwo/CaseStudyTwo.md *(not in the printed book)*
Solves: a quick tutorial: table with checkbox columns, adding tasks, a dialog to edit a task,
context menu with edit and delete.
Prerequisites: none
Defines: TodoTask TodoApplication TodoListPresenter TodoTaskPresenter
Sections:
- "A TodoTask class"
- "Creating your application" — SpApplication
- "Showing tasks" — SpCheckBoxTableColumn SpStringTableColumn | newTable addColumn: width:
- "About the layout definition"
- "Opening the application" — run
- "Improving the window"
- "Alternate initializePresenters method"
- "Making checkbox columns actionable" — onActivation: onDeactivation:
- "Adding new tasks to your list" — updatePresenter
- "Introducing a dialog" — addButton:do: openModal
- "Using the dialog" — newApplication:
- "Add edit and delete menu items" — newMenu addItem: contextMenu:
- "Conclusion"
Image examples: `SpTablePresenter class>>exampleWriteableTable`, `SpDemoCheckboxesPresenter`
Stale: none

## Tips and how tos — Chapters/Tips/Tips.md *(not in the printed book)*
Solves: sortable table, reusing commands, alternate row colors, initializing widgets from
values, Roassal inside, loading icons, custom drawing, migrating from Spec 1.0.
Sections: "Table with sorting", "Command reuse", "Alternate color",
"How to use your values to initialize your widgets", "Roassal inside", "How to load icons?",
"Can we draw custom widgets with Spec", "Migrating from Spec 1.0"
Image examples: `SpTablePresenter class>>exampleSorting`
Stale: `SpToolBarButton` is now `SpToolbarButtonPresenter`; `SpecLayout` and `newRow:height:`
are Spec 1.0 API (shown in "Migrating from Spec 1.0"). Check every snippet before using it.

## Widgets — Chapters/Widgets/Widgets.md *(not in the printed book; a draft)*
Solves: notebook (tabbed pages), studied on the Iceberg repositories browser.
Sections: "Notebook by example" — newNotebook addPageTitle:provider:
Image examples: `SpNotebookPresenter class>>example`, `SpDemoNotebookPresenter`
Stale: draft fragment only.

---

## Reverse index (API → chapter "section")

Classes
- CmCommand → Commander2 "Commands", "Defining commands"
- CmCommandGroup → Commander2 "Turning commands into menu items", "Managing a menubar", "Introducing groups", "Managing a toolbar"
- SpAbstractBackendForTest → TestingInSpec "Testing presenters"
- SpAlertDialog, SpConfirmDialog → ManagingWindow "Little modal dialog presenters"
- SpApplication → CaseStudyOne "Application"; InANutshell "Application"; Style "Defining an application and its style"; TestingInSpec "Testing your application"
- SpAthensPresenter → AthensRoassal "Direct integration of Athens with Spec"
- SpBoxLayout, SpBoxConstraints → LayoutContruction "BoxLayout (SpBoxLayout and SpBoxConstraints)", "Box alignment example"
- SpCodePresenter → Reuse "Changing the layout of a reused widget"; DynamicPresenter "Toggling Edit/Read-only mode"
- SpComponentListPresenter → ListTreeTable "Component lists"
- SpDialogWindowPresenter → ManagingWindow "Opening a window or a dialog box", "Acting on window opening"
- SpFilteringListPresenter, SpFilteringSelectableListPresenter → ListTreeTable "Filtering lists", "Selectable filtering lists"
- SpGridLayout, SpGridLayoutBuilder → LayoutContruction "Grid layout (SpGridLayout)"; CaseStudyOne "FilmPresenter"
- SpListPresenter → ListTreeTable "Lists" … "Drag and drop"; Transmissions "A simple example"
- SpMenuBarPresenter → Menus "Adding a menubar to a window"
- SpMenuPresenter → Menus "Implementing message menu commands", "Adding a context menu to a presenter"
- SpModelPort → CaseStudyOne "Using transmissions"; Transmissions "A more advanced example"
- SpMorphicConfiguration → InANutshell "Application configuration"; CaseStudyOne "Styling the application"
- SpMorphPresenter → AthensRoassal "Using the morph with Spec"
- SpOutputPort, SpInputPort, SpSelectionPort, SpTextPort → Transmissions "Available ports"
- SpOverlayLayout → LayoutContruction "Overlay layout (SpOverlayLayout)"
- SpPanedLayout → LayoutContruction "Paned layout (SpPanedLayout)"; MailApp "The `MailClientPresenter`"
- SpPresenter → ThreePillarsOfSpec "Example with SpPresenter"; FirstContact "Create the class of the UI"
- SpPresenterWithModel → ThreePillarsOfSpec "SpPresenter vs. SpPresenterWithModel", "Example with SpPresenterWithModel"; MailApp "The `EmailPresenter`"
- SpRoassalPresenter → AthensRoassal "SpRoassalPresenter"; Inspector "Adding Roassal charts"
- SpStatusBarPresenter → Menus "Adding a status bar to a window"
- SpStyle, SpStyleVariableSTONReader, SpPropertyStyle … → Style "Stylesheet examples", "Anatomy of a style"
- SpTablePresenter, SpStringTableColumn, Sp…TableColumn → ListTreeTable "Tables" … "Editable tables"; CaseStudyOne "List of films"
- SpTabLayout → InANutshell "Layouts"
- SpTreePresenter → ListTreeTable "Trees"; MailApp "The `MailAccountPresenter`"
- SpTreeTablePresenter → ListTreeTable "Tree tables"
- SpWindowPresenter → FirstContact "Define a title and window size, open and close the UI"; ManagingWindow "Window size and decoration"
- SpWindowWillClose → ManagingWindow "Preventing window close"

Selectors
- initializePresenters / connectPresenters / defaultLayout → ThreePillarsOfSpec (anchors sec_initializeWidgets, sec_connectPresenter, sec_layoutmethod)
- initializeWindow: → FirstContact "Define a title and window size, open and close the UI"; Menus "Adding a menubar to a window"
- initializeDialogWindow: → ManagingWindow "Acting on window opening"
- instantiate: / newPresenter: → FirstContact "Instantiate and configure subpresenters"; Reuse "Managing three widgets and their interactions"; CaseStudyOne "Opening presenters via the application"
- open / openWithLayout: / asWindow → FirstContact; ManagingWindow "Opening a window or a dialog box"; Reuse "Having different layouts"
- openDialog / openModal → ManagingWindow "Opening a window or a dialog box"; CaseStudyOne "Opening FilmPresenter in a modal dialog"
- okAction: / cancelAction: / addButton:do: / isOk → ManagingWindow "Getting values from a dialog window", "Placing a presenter inside a dialog window"
- whenWillCloseDo: / whenClosedDo: / whenOpenedDo: → ManagingWindow "Preventing window close", "Acting on window close", "Acting on window opening"
- focusOrder / takeKeyboardFocus → ManagingWindow "Setting keyboard focus"
- layout: / add:expand: / add:withConstraints: → LayoutContruction "BoxLayout (SpBoxLayout and SpBoxConstraints)"; DynamicPresenter "Layouts as simple as objects"
- remove: / presenters → DynamicPresenter "Defining add/remove buttons"
- setModel: / model: / on: / newApplication:model: → ThreePillarsOfSpec "SpPresenter vs. SpPresenterWithModel"
- whenSelectionChangedDo: / selectedItem → ListTreeTable "About single/multiple selection"; Reuse "Supporting reuse"
- whenTextChangedDo: / whenSubmitDo: → InANutshell "Navigation between presenters"; MailApp "The `EmailPresenter`"
- display: / displayIcon: / sortingBlock: → ListTreeTable "Controlling item display", "Decorating elements"
- dragEnabled: / dropEnabled: / wantsDrop: / acceptDrop: → ListTreeTable "Drag and drop"
- roots: / children: / expandPath: → ListTreeTable "Trees"
- addColumn: / title:evaluated: / sortFunction: / beEditable → ListTreeTable "First table", "Sorting headers", "Editable tables"
- transmitTo: / transmitTo:transform: / transmitDo: / postTransmission: → Transmissions "Basic transmission" … "Acting after a transmission"
- newMenuBar / newMenu / addItem: / addGroup: / subMenu: → Menus "Adding a menubar to a window", "Implementing message menu commands"
- contextMenu: → Menus "Adding a context menu to a presenter"; ListTreeTable "Trees"
- newToolbar / newToolbarButton / toolbar: → Menus "Adding a toolbar to a window"; CaseStudyOne "Improving the window"
- newStatusBar / pushMessage: → Menus "Adding a status bar to a window"
- addKeyBindingsTo: → Menus "Installing shortcuts" (written `addKeybindingsTo:` in the book)
- asSpecCommand / asSpecGroup / fillWith: / iconName: / shortcutKey: → Commander2 "Managing icons and shortcuts", "Using fillWith:"
- buildMenuBarGroupWith: / buildToolBarGroupWith: → Commander2 "Managing a menubar", "Managing a toolbar"
- addStyle: / removeStyle: / styleSheet: / addStyleSheetFromString: → Style "Applying styles", "Dynamically applying styles"; CaseStudyOne "Styling the application"
- useBackend:with: / configure: → InANutshell "Application configuration"; CaseStudyOne "Styling the application"
- drawBlock: → AthensRoassal "Direct integration of Athens with Spec"
- withoutEvaluator → Inspector "Removing the evaluator"
- click / clickAtIndex: → TestingInSpec "Tests"

Not covered by the book (use the image examples)
- Actions API: `addActionWith:`, `addShortcutWith:`, `actions:`, `SpAction` (package `Spec2-Commander2`)
- `SpListViewPresenter`, `SpTreeListViewPresenter` (package `Spec2-ListView`)
- Notebooks, radio buttons, toggle buttons, progress bars, popovers: `SpDemo` pages
  (`SpDemoNotebookPage`, `SpDemoRadioButtonsPage`, `SpProgressBarDemoPage`, `SpPopoverExample`, `SpToggleButtonExample`)
