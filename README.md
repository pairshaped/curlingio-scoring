# Scoring for [Curling I/O](https://curling.io)

## Features

* Lists games in their draw schedule. Possibly use a list view instead of the table / grid for phone compatibility.
* Games can be clicked / touched which will replace the view with the game to be updated.
* First class support for phones and tablets so that curlers can enter their own scores provided they're logged in with an account that has permissions.
* Performant, fast UI switching and only updates the game on save (nothing else).
* Game names can be changed if the user has appropriate permissions (access to stages).
* Validations. Make sure game names are unique across the entire event. Make sure scores aren't astronomical or negative.

## Roadmap

* Line scores (enabled via settings)
* Shot by shot (enabled via settings)
* Websockets for better refresh / multi-client performance.

## Installing Dependencies

```
yarn
```

## Running It

```
yarn start
```

## Source
<https://github.com/pairshaped/curlingio-scoring>

## Copyright and License

Curling I/O Scoring
Copyright (C) 2019 Curling I/O

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
