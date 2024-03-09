# ABAP Comment System

Attach a commenting system to any SAP application/object/transaction. <BR />
Let the users chat about documents, take note and like comments as in a blog post comments feed.

## Requirements
ABAP 7.40

## Usage
Create a comment system object anywhere, using an `object_id` identifying the context to which the comments will be attached: it can be the transacion/application name, a key for a SAP document or a custom object,
it can a be a string but also a structure or a more complex object. Keep in mind: only one corresponding comment system will be loaded for each object id.

```abap
DATA(comment_system) = zcl_cmt_system=>create_comment_system( object_id ).
```

Trigger the comment system by calling `display` method

```abap
comment_system->display( ).
```

Comments will pop-up in a dedicated window. Using the optional `settings` parameter you can control pop-up coordinates and title.

<p align="center"><img src="/docs/ex1.png" width="500"></p>

If you use the SAP GUI, in order to properly display the css style, I suggest you to set "Edge" as HTML control for GUI, instead of "Internet Explorer".

## Features
1. Any user can like any comment
2. Each user can delete its own comment
3. Comment author name is the user full name (if empty, the user id will be used)
4. The avatar contains user name initials with a random background
5. Get the number of unread comments by calling `comment_system->notify_to_read( )`, so you can e.g. trigger the comment by means of a button with dynamic text "Comment (3 to read)" or something like this
6. Don't you like pop-ups? Create a custom container for comment section and a custom container for textarea, then display comments by calling
   ```abap
   comment_system->display_in_container(
     section_container   = `C_SECTION`
     textarea_contaienr  = `C_TEXTAREA` ).
   ```
   
   
## Installation
Install this project using [abapGit](https://abapgit.org/) ![abapGit](https://docs.abapgit.org/img/favicon.png)
