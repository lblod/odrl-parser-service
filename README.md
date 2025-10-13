# Convert ODRL policies to authorisation rules

This service supports converting authorisation rules expressed as [ODRL](https://www.w3.org/TR/odrl-model/) policies to corresponding rules for the [sparql-parser](https://github.com/mu-semtech/sparql-parser) service.

This service is currently under development, this README will be extended along the way.

## Installation
Add the service to your project as follows:
```yaml
odrl-parser
  image: lblod/odrl-parser-service
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

- support for is live-reloading is *experimental*;
- the services does *not* come with a web framework, one will be added when the need arises; and
- it does not come with the usual helpers. Instead its functionality to interact with a triplestore was copied from [mu-cl-resources](https://github.com/mu-semtech/mu-cl-resources) and relies on [mu-cl-support](https://github.com/mu-semtech/mu-cl-support).


Given these limitations there are two main options for local development:

1. Run the service in a docker container using live reloading to restart the REPL after changes. This is the easiest to set up, but since the REPL is fully restarted after a change it does not have the usual flexibility.
2. Run the service in a project-local REPL on your host machine and forward its requests to a running stack. This is harder to setup, but has the advantage you can develop and interact with the REPL as usual.


### REPL in container using live reload
:warning: The live reloading functionality is currently highly experimental. Please report any encountered problems. Suggestions for improvements are always welcome.

The live reloading functionality will restart a container's REPL whenever a `.lisp`, `.asd`, or `.lock` file changes. To enable live reloading set the service's `MODE` environment variable to `"development"` and mount the project's host folder as volume:

```yaml
odrl-parser:
  environment:
    MODE: "development"
  volumes:
    - /host/path/to/odrl-parser-service/:/app/
```

#### Dependency management
This project uses [qlot](https://github.com/fukamachi/qlot) to manage its dependencies. You can either install `qlot` on your host machine following their [README's instructions](https://github.com/fukamachi/qlot?tab=readme-ov-file#installation), or manage dependencies from within the service's container using the qlot instance installed there.

If you installed `qlot` on your host machine you can use `qlot add PKG-NAME` in the project's root folder to install new dependencies. This command will add entries to the `qlfile` and `qlfile.lock` files. Changes to the latter file will trigger live reloading, thereby making the installed dependency available in the container's REPL.

If your prefer not to install `qlot` on your host, you can manage dependencies from within the container. To do this, open a shell in the container and `cd` to the `/app` directory. There you can use `qlot` to install additional packages:

```shell
# On the host
$ docker compose exec odrl-parser bash
# In the container
$ cd /app
$ qlot add PKG-NAME
```

Same as using `qlot` on your host, the `qlot add` command will update `qlfile` and `qlfile.lock` files, triggering live reload.


### Project-local REPL on host with request forwarding
In order to develop this service fully on your host machine you will need to set up a project-local REPL and forward its request to a running stack. This requires installing [qlot](https://github.com/fukamachi/qlot) to manage the project's dependencies and adding [docker-forward](https://github.com/madnificent/docker-forward) to a stack to forward requests to it.

To install `qlot` consult the [instructions](https://github.com/fukamachi/qlot?tab=readme-ov-file#installation) in their README. Make sure the `qlot` binary is accessible by your editor, as it will be necessary to start a project-local REPL.

Step by step instructions:

1. Clone this repository and `cd` to its directory.
2. Install the necessary dependencies by running `qlot install`.
3. Start a project-local REPL, see the corresponding entry in the [qlot README](https://github.com/fukamachi/qlot?tab=readme-ov-file#working-with-slime) for more details on how to do this in different editors.
4. Load this service's system using by executing `(asdf:load-system "odrl-parser")` in the project-local REPL.
5. Add the `docker-forward` service to the stack you want to interact with by adding the following to its docker compose configuration. Make sure to `up` the added forward service.

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
