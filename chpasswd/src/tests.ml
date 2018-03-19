open OUnit2

let tests = "all tests" >::: [
    ControllerTest.tests;
    DatabaseTest.tests;
    DoveadmTest.tests;
    MailsTest.tests;
    ModelDatabaseTest.tests;
    ModelTest.tests;
    TemplateTest.tests;
    TokenTest.tests;
    ViewCgiTest.tests;
    ViewWebTest.tests
  ]
let () = run_test_tt_main tests
