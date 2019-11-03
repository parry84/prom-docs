prom-docs - a metrics reference generator for Prometheus

```
Usage: prom-docs [-o|--output OUTPUT] [-c|--config CONFIG] [-v|--verbose]
  Prometheus metrics reference generator

Available options:
  -o,--output OUTPUT       Write to a file instead of stdout
  -c,--config CONFIG       Specify a configuration file (default is input.yaml)
  -v,--verbose             Make the operation more talkative
  -h,--help                Show this help text
```

The configuration file is a Yaml file that list the Prometheus sources to be scraped. Example:

```
- source: local
  path: "./os.metrics"
- source: local
  path: "./api.metrics"
- source: local
  path: "./engine.metrics"
- source: local
  path: "./scheduler.metrics"
- source: local
  path: "./queue.metrics"
- source: local
  path: "./db.metrics"
```