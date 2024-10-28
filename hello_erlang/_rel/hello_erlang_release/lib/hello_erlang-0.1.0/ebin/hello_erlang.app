{application, 'hello_erlang', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['hello_erlang_app','hello_erlang_sup']},
	{registered, [hello_erlang_sup]},
	{applications, [kernel,stdlib]},
	{optional_applications, []},
	{mod, {hello_erlang_app, []}},
	{env, []}
]}.