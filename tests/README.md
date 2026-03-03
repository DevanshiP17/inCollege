# View Network Test Cases

## Positive Test 1: network lists accepted user connection(s)

# Conditions

make sure connections.dat has an ACCEPTED connection between two users. Make sure accounts.dat has the two users logged.

## Negative Test 1: Connections exist, but no ACCEPTED connections for the user logged in

### Conditions

make sure connections.dat exists and there exists connections between users, but no ACCEPTED connection to or from logged in user. Make sure accounts.dat has all user logins used

## Edge Test 1: delete connections.dat

### Conditions

make sure accounts.dat has user info and delete connections.dat if it exists

## Edge Test 2: logged in user is receiving connection

### Conditions

make sure accounts.dat has both user logins and add a connection from other user to logged in user with code "ACCEPTED"


