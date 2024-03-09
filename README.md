# ABAP Comment System

Attach a commenting system to any SAP application/object/transaction. <BR />
Let the users chat about documents, take note and like comments as in a blog post comments feed.

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

## Features
1. Any user can like any comment
2. Each user can delete its own comment
3. Comment author name is the user full name (if empty, the user id will be used)
4. The avatar contains user name initials with a random background
