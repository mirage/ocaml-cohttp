#!/usr/bin/env node

const assert = require("assert");
const { XMLHttpRequest } = require("xmlhttprequest");

global.XMLHttpRequest = XMLHttpRequest;

const tests = require("./cohttp_lwt_jsoo_test.bc.js");

async function main() {
  {
    const [status, _body] = await tests.request("https://mirage.io");
    assert(status === 200);
  }
  {
    const [status, _body] = await tests.request(
      "https://this.domain.does.not.exist"
    );
    assert(status === 0);
  }
}

main();
