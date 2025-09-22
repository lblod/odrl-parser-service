# Convert ODRL policies to authorisation rules

This service supports converting authorisation rules expressed as [ODRL](https://www.w3.org/TR/odrl-model/) policies to corresponding rules for the [sparql-parser](https://github.com/mu-semtech/sparql-parser) service.

This service is currently under development, this README will be extended along the way.

## Installation
Add the service to your project as follows[^1]:
```yaml
odrl-parser
  image: lblod/odrl-parser-service:feature-initial-implementation
  volumes:
    - ./config/odrl-parser:/config
```


## API
### `GET /load-policy`
Insert the triples for the ODRL policy defined in `config/config.nt` into the backend's triplestore.

### `GET /generate-config[?policy-name=NAME]`
Generate the sparql-parser configuration for a stored ODRL policy `NAME`. Here `NAME` should be the last past of the URI of a `odrl:Set` resource. If no policy name is provided the service will generate a configuration for each `odrl:Set` resource it finds in the database.

Any generated configuration files are written to the mounted volume. As filenames the last part of the resource's URI is used, same value as one would provide as `NAME` argument.


## Local development
In contrast to most other mu-services this service is not developed based on a template, since there is currently no common lisp template available. Consequently, developing this service currently requires a bit more setup than usual. Currently, the major differences are:

- live-reloading is *not* supported;
- the services does *not* come with a web framework, one will be added when the need arises; and
- it does not come with the usual helpers. Instead its functionality to interact with a triplestore was copied from [mu-cl-resources](https://github.com/mu-semtech/mu-cl-resources) and relies on [mu-cl-support](https://github.com/mu-semtech/mu-cl-support).

Given these limitations the most convenient way to develop on this service is to do so in a project-local REPL using
- [qlot](https://github.com/fukamachi/qlot) to manage the project's dependencies, and
- [docker-forward](https://github.com/madnificent/docker-forward) to forward requests to a running stack.

Step by step instructions:
- Clone this repository and `cd` to its directory.
- Install the necessary dependencies by running `qlot install`[^2]
- Start a project-local REPL, see the corresponding entry in the [qlot README](https://github.com/fukamachi/qlot?tab=readme-ov-file#working-with-slime) for more details on how to do this in different editors.
- Load this service's system using by executing `(asdf:load-system "odrl-parser")` in the project-local REPL.
- Add the `docker-forward` service to the stack you want to interact with by adding the following to its docker compose configuration. Make sure to `up` the added forward service.

```yaml
forward:
  image: madnificent/forward
  environment:
    TARGET_PORT: 8890
  ports:
    - "8896:80"
```

Note that `8896` is the host port from which the service will send requests for the triplestore as configured in the [settings file.](https://github.com/lblod/odrl-parser-service/blob/feat/initial-implementation/settings.lisp#L26). If you want to use a different port make sure to update the port in the settings file as well.


### Tests
The files in the `tests` folder provide some, rudimentary, functionality to simplify manually testing parts of the service. Each file provides functions to create object instances for the different classes defined by this service. These can be used to manually test individual steps in the conversion flow.

Note, these files are not **not** automatically loaded and their functions are **not** exported. Before using these functions, make sure to load the file(s) in question.


[^1]: Since the service is still in the alpha development phase no tagged version is currently available.

[^2]: Consult [qlot](https://github.com/fukamachi/qlot) for more information on how to install `qlot` and how it works.
