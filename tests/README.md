# View Messages Test Cases

## Positive Test 1:  a user can view exactly one received message.

### Conditions

messages.dat has: alice|TestUser|2026-04-06 07:56:00|Hi TestUser!

## Positive Test 2:  a user can view multiple messages from different senders.

### Conditions

messages.dat has :
alice|TestUser|2026-04-06 07:56:00|Good Afternoon
NewStudent|TestUser|2026-04-06 08:01:00|Hey how are you.


## Negative Test 1: messages.dat exists with messages, but no messages to logged in user

### Conditions

mesages.dat has:
alice|NewStudent|2026-04-06 08:00:00|Hey guy
NewStudent|alice|2026-04-06 08:10:00|hey


## Negative Test 2: system shows no messages with no messages.dat file

### Conditions

delete messages.dat

## Negative case 3: make sure outgoing messages don't show up in view my messages

### Conditions

TestUser|alice|2026-04-06 11:00:00|hey alice
alice|TestUser|2026-04-06 08:17:00| hey test user

## Edge case 1: make sure system matches username with case insensitivty

### Conditions

messages.dat:
alice|testuser|2026-04-06 08:22:00| testing case insensitivity

## Edge case 2: make sure system still works with blank messages

### Conditions

messages.dat:
alice|TestUser|2026-04-06 08:28:00| 
