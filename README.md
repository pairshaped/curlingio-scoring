# Scoring for [Curling I/O](https://curling.io)

![Scoring for Curling I/O](curlingio-scoring.gif?raw=true "Scoring for Curling I/O")

## Features

* Lists games in their draw schedule.
* Games can be clicked / touched which will replace the view with the game to be updated.
* First class support for phones and tablets so that curlers can enter their own scores provided they're logged in with an account that has permissions.
* Performant, fast UI switching and only updates the game on save (nothing else).
* Game names can be changed if the user has appropriate permissions (access to stages).
* Validations. Make sure scores aren't astronomical or negative.
* End scores (line scores).
* Setting first hammer (last shot first end).
* Setting rock colors.
* Ends are added automatically if tied after regulation (10 ends). Manually setting the result to tied will remove extra end(s).
* Shot by shot (enabled via league / competition settings)

## For Contributors

### Installing Dependencies

We use elm and elm-live for development. You can install these via npm.

```
npm install
```

### Running It

Edit dev.html and configure the application's parameters for your environment. Then run it:

```
npm start
```

### Production Deployment

Make sure you have uglify-js installed to compress the production js.
```
npm install -g uglify-js
```

Compile and optimize for production using:

```
./prod.sh
```

## Source
<https://github.com/pairshaped/curlingio-scoring>

## Copyright and License

[See LICENSE.md](LICENSE.md)
