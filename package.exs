defmodule Maestro.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :gnaw,
     version: @version,
     description: "An Erlang pool of worker pools.",
     package: package]
  end

  defp package do
    [files: ~w(src rebar.config README.md LICENSE VERSION),
     contributors: ["Guilherme Andrade"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/g-andrade/maestro"}]
  end
end
